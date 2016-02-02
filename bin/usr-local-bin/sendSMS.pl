#!/usr/bin/perl
##########################################################################
$prognfo = <<EOM;

sendSMS.pl  v0.2.2     10/04/2004
copyright (c) 2001-2004 by Paul Kreiner

EOM
#
# Written by: Paul Kreiner <deacon@thedeacon.org>
#   with portions by: Brandon Zehm <caspian@dotconf.net>
#
# History:
# The original sendSMS.pl was an personal-use hack written
#   by Brandon Zehm.  It has been completely rewritten to
#   be modular, robust, and extendable, and to easily allow the
#   addition of other service providers.
#
# Changelog:
# v0.2.2     10/04/2004
#   Incorporated change to ATTWS profile to reflect changes on their website.
#   (reported and patch provided by Joel Chen)
#   Paul Kreiner
#
# v0.2.1     09/25/2004
#   Added support for HTTPCookie parameter for each provider. Currently, only
#   SPRINT requires a cookie to be set, but others may need it later.
#   Incorporated changes and features to support Sprint PCS now that they've
#   changed their website again (thanks to Joel Chen for his help).
#   Paul Kreiner
#
# v0.2.0     05/13/2004
#   Incorporated patch to add Bell Canada / Bell Mobility support (provided
#   by David Caplan).
#   Removed support for deprecated 'VSTREAM' service provider. NOTE: If you
#   are still using 'VSTREAM', you must switch to 'TMOBILE'.
#   Paul Kreiner
#
# v0.1.9     12/12/2003
#   Incorporated change to Sprint PCS profile to reflect changes on their
#   website - they only send 'Message sent' now (reported by Ken Cochran).
#   Added global version/date/copyright variables to code so help screen
#   gives program info without having to modify it in multiple places.
#   Simplified the help screen by removing "written by:" and extra CR's.
#   Paul Kreiner
#
# v0.1.8     09/15/2003
#   Incorporated patch to add Sprint PCS support (provided by John Carbone).
#   Paul Kreiner
#
# v0.1.7     09/04/2003
#   Incorporated patch to add T-Mobile support (provided by Manuel Martin).
#   Incorporated patch to add Cricket support (provided by Tod Morrison).
#   Marked VoiceStream support as deprecated in help and code comments.
#   Small fixes for typos and minor cosmetic code cleanups.
#   Paul Kreiner
#
# v0.1.6     08/12/2002
#   Updated ATTWS profile to reflect changes on their website.
#   Jared Cheney
#
# v0.1.5     05/29/2002
#   Updated to support web proxies.
#   Brandon Zehm
#
# v0.1.4     04/04/2002
#   Updated VoiceStream Support and added support for SkyTel pagers.
#   Brandon Zehm
#
# v0.1.3     01/22/2001
#   Some changes to get this running on Win32 (ActiveState 5.6.0/build 623).
#   Couldn't get POSIX::termios or IO::Select to correctly make STDIN non-
#   blocking, so I gave up after ~4 hrs of trying, and simply removed the
#   STDIN message-sending functionality for Win32.  If someone out there
#   can send me a patch (and NOT simply include an external module, such as
#   Term::ReadKey or something) then maybe it'll be supported.
#   Paul Kreiner
#
# v0.1.2     01/20/2001
#   Added support for passing a message via STDIN.  Got the list of providers
#   in the help sub to be dynamically generated, making it that much easier to
#   add service providers (only two places to edit, now.)  Implemented signal
#   trapping for cleaner exits.  Added support for Cellular One (unverified).
#   Minor code cleanups.
#   Paul Kreiner
#
# v0.1.1     01/17/2001
#   Update to improve modularity and make adding new providers much easier
#   than before.   Finished removing any global vars, just because I can.
#   Replaced original encodeAsURL code with much more efficient algorithm.  I
#   think that was the last of Brandon's original code.
#   Paul Kreiner
#
# v0.1.0     01/15/2001
#   Initial 'beta' release.  Basic functionality in place, although modularity
#   needs to be improved.  This is a full rewrite of the original sendSMS.pl
#   script, with commentary, support for multiple providers, and a more robust
#   user interface.  Includes support for AT&T WS and VoiceStream (verified).
#   Paul Kreiner
#
# Notes:
# It's a big script, for what it does.  Note that one of the primary
#   design goals was to have a single self-contained script that
#   would do everything -- no CPAN modules, .conf files, etc.
#   The layout of the script is set up so the templates for various
#   service providers appear right near the top, so if someone
#   (yeah, this means YOU) wants to add/modify support for
#   a service provider, it should be really easy to see all the fields
#   to modify.
#
# License:
# sendSMS.pl (hereafter referred to as "program") is free software;
#   you can redistribute it and/or modify it under the terms of the GNU General
#   Public License as published by the Free Software Foundation; either version
#   2 of the License, or (at your option) any later version.
# Note that when redistributing modified versions of this source code, you
#   must ensure that this disclaimer and the above coder's names are included
#   VERBATIM in the modified code.
#
# Disclaimer:
# This program is provided with no warranty of any kind, either expressed or
#   implied.  It is the responsibility of the user (you) to fully research and
#   comprehend the usage of this program.  As with any tool, it can be misused,
#   either intentionally (you're a vandal) or unintentionally (you're a moron).
#   THE AUTHOR(S) IS(ARE) NOT RESPONSIBLE FOR ANYTHING YOU DO WITH THIS PROGRAM
#   or anything that happens because of your use (or misuse) of this program,
#   including but not limited to anything you, your lawyers, or anyone else
#   can dream up.  And now, a relevant quote directly from the GPL:
#
#                           NO WARRANTY
#
#  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
# FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
# OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
# PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
# OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
# TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE
# PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
# REPAIR OR CORRECTION.
#
#   ---
#
# Whee, that was fun, wasn't it?  Now let's all get together and think happy
#   thoughts.  And remember, the same class of people who made all the above
#   legal spaghetti necessary are the same ones who are stripping your rights
#   away with DVD CCA, DMCA, stupid software patents, and retarded legal
#   challenges to everything we've come to hold dear about our internet.
#   So enjoy your dwindling "freedom" while you can, because 1984 is coming
#   sooner than you think.  :[
#######################################
# Some quick function definitions (in order of appearance):
#
# sub getProvider:    names & parameters of service providers. USER MODIFYABLE.
# sub initialize:     sets up initial SIG handlers & alarms
# sub run:            the main loop of the program
# sub processCmdLine: parse STDIN and command line for user input
# sub checkInput:     sanity check of user-input values
# sub encodeAsURL:    encodes any string(s) as legit URL(s)
# sub openSocket:     opens a socket to remote webserver
# sub sendData:       build & send HTTP request to remote server
# sub waitforReply:   waits for success/fail response from remote server
# sub printHelp:      print user help message & list of providers
# sub exit:           exit, print optional error message and close filehandles
######################################

# Here we go...
use vars qw($prognfo);
use strict;
initialize();     ### set up some initial SIG handlers, etc.
run();            ### executes the main body of the program (done for readability)
exit(1);          ### execution should never get here, but just in case...


############
# Function: getProvider( \$HTTPRequestMethod,
#   \$HTTPRequestString, \$HTTPReferer, \$HTTPCookie, \$remoteHostName,
#   \$formDataString, \$successString, \$provider, \$receiver,
#   \$sender, \$message )
# Pass this function several references to scalars, as follows:
#   -  $HTTPRequestMethod is either "GET" or "POST", depending
#   upon what is specified in the selected provider's template.
#   -  $HTTPRequestString is the rest of the GET or POST line that
#   is to be sent to the remote server.  Again, part of the template.
#   -  $HTTPReferer is the referer string defined in the provider's template.
#   -  $HTTPCookie is the cookie string defined in the provider's template.
#   Note that the cookie string can (and should) be left empty, unless you
#   *know* that your provider requires a cookie to work.
#   -  $remoteHostName is the name of the remote webserver.  It can also have
#   a "magic" value when the function is called, which will alter the behavior
#   of this function.  See below for more info.
#   -  $formDataString is the actual data, formatted differently per provider,
#   that will be submitted to the provider's website.  This string is defined
#   as part of each provider's template.
#   -  $successString is the string which we will wait to receive back
#   from the remote server, to signal that we were successful.  This is
#   defined differently for each provider, as part of their template.
#   -  $provider is the name of our selected service provider.
#   -  $receiver is the cellular number (10-digit) of our recipient.
#   -  $sender is the sender's name / number.
#   -  $messageBody is the text of the message body.
#
# This function will find the user-selected service provider's template
#   and fill in all other referenced variables with the correct info for that
#   provider.  This includes remote servername, HTTP request method,
#   referring webpage, success string, and a properly-formatted form
#   submission data string.
# There is a second use to this function -- if it is called with a magic value
#   of 'get providerNames' in $remoteHostName, it will return an array with
#   the names/descriptions of all available service providers.
###
sub getProvider
{
  (
  my $HTTPRequestMethod,
  my $HTTPRequestString,
  my $HTTPReferer,
  my $HTTPCookie,
  my $remoteHostName,
  my $formDataString,
  my $successString,
  my $provider,
  my $receiver,
  my $sender,
  my $messageBody,
  ) = @_;


##############################################################################
#
# BEGIN USER-MODIFYABLE SECTION:
#
##############################################################################

# List of providers by name.  First entry is the name of the service parameters
#   array.  Second entry is the "common" name of the service provider.  This is
#   the name that the provider will be referenced as on the command line.
#   Finally, the third entry is the full name of the service provider.  This is
#   used when the list of providers is printed (usually by the help function).
# NOTE: you will need to add your provider's info here, and below (in the
#   "parameters" section) to add support for them to sendSMS.  A sample entry
#   has been created for you.
my @providerNames = (
#  \my %TEMPLATE, 'TEMPLATE', 'An example service provider',
  \my %ATTWS, 'ATTWS', 'AT&T Wireless',
  \my %BELLCA, 'BELLCA', 'Bell Canada',
  \my %CELLONE, 'CELLONE', 'Cellular One',
  \my %CRICKET, 'CRICKET', 'Cricket',
  \my %SKYTEL, 'SKYTEL', 'SkyTel',
  \my %SPRINT, 'SPRINT', 'Sprint PCS',
  \my %TMOBILE, 'TMOBILE', 'T-Mobile',
);

if ($remoteHostName == 'get providerNames')
{   #return a list of providers immediately if being called with a magic
    #  value for remoteHostName
  return(@providerNames);
}

# Service provider's PARAMETERS section.  Note that an example TEMPLATE
#   provider entry is already here.  I suggest using a sniffer tool such as
#   ngrep or tcpdump to sniff the conversation between your browser and the
#   target webpage, then examining it to extract the relevant information for
#   use in this section.  For example, if a session to www.example.com runs as
#   follows:
#    POST /cgi-bin/sms.cgi HTTP/1.1
#    Referer: http://www.example.com/index.html
#    Host: www.example.com
#    Cookie: JSESSIONID=boguscookie
#    Content-type: application/x-www-form-urlencoded
#    Content-length: 53
#    sender=123&receiver=456&text=This is a test message
#
#   And a successful message returns a page with the text "Mesage submitted
#    successfully", then the data you'd insert below would be:

### An example service provider's parameters:
#%TEMPLATE = (
#  'remoteHostName'    => 'www.example.com',
#  'HTTPRequestMethod' => 'POST',
#  'HTTPRequestString' => '/cgi-bin/sms.cgi',
#  'HTTPReferer'       => 'http://www.example.com/index.html',
#  'HTTPCookie'        => 'JSESSIONID=boguscookie',
#  'successString'     => 'successfully',
#  'formDataString'    => 'sender=$$sender&receiver=$$receiver&text=$$messageBody',
#);

### AT&T Wireless service parameters:
%ATTWS = (
  'remoteHostName'    => 'www.mymmode.com',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/messagecenter/sendMessage.do',
  'HTTPReferer'       => 'http://www.mymmode.com/messagecenter/init',
  'HTTPCookie'        => '',
  'successString'     => 'submitted',
  'formDataString'    => 'recipient=$$receiver&sender=$$sender&subject=&message=$$messageBody',
);

### Bell Canada (Bell Mobility) service parameters:
%BELLCA = (
  'remoteHostName'    => 'www.txt.bellmobility.ca',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/sms/sendMessage',
  'HTTPReferer'       => 'http://www.txt.bellmobility.ca/sms/sendMessage',
  'HTTPCookie'        => '',
  'successString'     => 'essage was sent to',
  'formDataString'    => 'TEMPLATE=/img/sms/general/outbox/messageConfirmation.html&EXCEPTION_TEMPLATE=/img/sms/general/imgHtmlException.html&TRANSPORT_TYPE=sms&MIN_NUMBER=$$receiver&SMTP_RETURNPATH=&MIN_RETURNPATH=&FROM=&CALLBACK_NUMBER=&STATUS_REQUEST=&MESSAGE_SUBJECT=$$sender&MESSAGE_BODY=$$messageBody&CHARCOUNT=150&x=100&y=15',
);

### Cellular One service parameters:
%CELLONE = (
  'remoteHostName'    => ' www.celloneusa.com',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/cgi-bin/sendMsg.pl',
  'HTTPReferer'       => 'http://www.celloneusa.com/message.cfm',
  'HTTPCookie'        => '',
  'successString'     => 'will be sent',
  'formDataString'    => 'toPhone=$$receiver&fromPhone=$$sender&SendTime=0&FormName2=Short\%20Messaging\%20System&FileName2=Message.pl&Required2=toPhone\%2BfromPhone\%2BtxtMessage&txtMessage=$$messageBody'
);

### Cricket service parameters:
%CRICKET = (
  'remoteHostName'    => 'www.mycricket.com',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/text/sendto_sms_system.asp?Step=2',
  'HTTPReferer'       => 'http://www.mycricket.com/text/sendto_sms_system.asp',
  'HTTPCookie'        => '',
  'successString'     => 'Message Sent',
  'formDataString'    => 'cricket_phone_number=$$receiver&msg=$$messageBody&from=$$sender'
);

### SkyTel service parameters:
%SKYTEL = (
  'remoteHostName'    => 'www.skytel.com',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/servlet/SendMessage',
  'HTTPReferer'       => 'http://www.skytel.com:80/servlet/SendMessage',
  'HTTPCookie'        => '',
  'successString'     => 'Received Your Message',
  'formDataString'    => 'cmd=post&recipients=$$receiver&message=$$messageBody'
);

### Sprint PCS service parameters:
%SPRINT = (
  'remoteHostName'    => 'messaging.sprintpcs.com',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/textmessaging/composeconfirm',
  'HTTPReferer'       => 'http://messaging.sprintpcs.com/textmessaging/compose',
  'HTTPCookie'        => 'JSESSIONID=boguscookie',
  'successString'     => 'Message Sent',
  'formDataString'    => 'phoneNumber=$$receiver&message=$$messageBody&characters=160&callBackNumber=$$sender'
);

### T-Mobile (was: VoiceStream) service parameters:
%TMOBILE = (
  'remoteHostName'    => 'www.t-mobile.com',
  'HTTPRequestMethod' => 'POST',
  'HTTPRequestString' => '/messaging/default.asp',
  'HTTPReferer'       => 'http://www.t-mobile.com/messageing/default.asp',
  'HTTPCookie'        => '',
  'successString'     => 'message was sent to',
  'formDataString'    => 'txtNum=$$receiver&txtFrom=$$sender&txtMessage=$$messageBody'
);



# Check to be sure we know how to send to the selected service provider...
# We run through all the service providers, looking for a match by name:
for (my $count=0; $count < @providerNames; $count+=3)
{
  if ($$provider =~ /^$providerNames[$count + 1]$/i)
  {   #We found a match - let's copy the data out of the template.
    $$remoteHostName= % { $providerNames[$count] } -> { 'remoteHostName' };
    $$HTTPRequestMethod= % { $providerNames[$count] } -> { 'HTTPRequestMethod' };
    $$HTTPRequestString= % { $providerNames[$count] } -> { 'HTTPRequestString' };
    $$HTTPReferer= % { $providerNames[$count] } -> { 'HTTPReferer' };
    $$HTTPCookie= % { $providerNames[$count] } -> { 'HTTPCookie' };
    $$successString= % { $providerNames[$count] } -> { 'successString' };
    # A little tricky here.  Because the contents of formDataString include
    #   dynamic content (sender, recipient, message), we need to substitute the
    #   real data in place of the variable names, before assigning the entire
    #   string to $$formDataString to be used elsewhere.  Eval() allows this.
    eval "\$\$formDataString=\"" . %{$providerNames[$count]} -> { 'formDataString' } . "\"";
  }
}

# Unknown service provider.  Exit with an error description.
  if (! $$formDataString )   # We couldn't find a service provider match!
	{ &printHelp;
	  &exit("ERROR: the service provider ($$provider) you specified is not recognized.");
	}
}


##############################################################################
# END USER-MODIFYABLE SECTION.
# You shouldn't need to touch anything past this point, if you simply wish
#   to add support for another service provider.
##############################################################################
























# This space intentionally left blank.
























############
# Function: initialize()
# Sets up anything/everyting before the program's execution really starts.
#   This is basically to keep things tidy up at the top of the source.
#   Everything that would normally be up there is included here, instead.
###
sub initialize
{
  ### Intercept signals and exit cleanly ###
  $SIG{'QUIT'} = sub { &exit("ERROR:  Exiting prematurely...received SIG$_[0]!"); };
  $SIG{'INT'} = sub { &exit("ERROR:  Exiting prematurely...received SIG$_[0]!"); };
  $SIG{'KILL'} = sub { &exit("ERROR:  Exiting prematurely...received SIG$_[0]!"); };
  $SIG{'TERM'} = sub { &exit("ERROR:  Exiting prematurely...received SIG$_[0]!"); };

  if ($^O !~ /win/i) {          ### SIGHUP and SIGALRM are not supported on Win32
    $SIG{'HUP'} = sub { &exit("ERROR:  Exiting prematurely...received SIG$_[0]!"); };
    $SIG{'ALRM'} = sub { &exit("FAILURE:  Execution ran over 30-second limit.  Exiting..."); };
    alarm(30);   # Abort execution if process runs over 30 seconds:
  }
}


############
# Function: run()
# This is the core of the program's execution.  It simply runs through
#   making calls out to other subroutines as the program executes.  This
#   was done primarily to keep the top of the code clean (for provider
#   templates).  Note that regardless of success or failure, the program
#   should exit from somewhere within run() or one of it's called subroutines.
#   In other words, run() should never return to main().
###
sub run
{
# Process the command line arguments to get the variables we're going to use,
#   and to catch any really glaring user errors.
  processCmdLine(
    \my $receiver,
    \my $provider,
    \my $sender,
    \my $data, 
    \my $proxy, 
    \my $proxyport );

# Sanity check of the value of our user-input variables.
  checkInput(
    \$provider,
    \$receiver);

# Convert relevant strings to URL-encoding before putting them into the request.
  encodeAsURL(
    \$sender,
    \$receiver,
    \$data);

# Parse the user's input to get back all the provider-specific info we'll need
#   later on.
  getProvider (
    \my $web_method,
    \my $web_methodstring,
    \my $web_referer,
    \my $web_cookie,
    \my $web_server,
    \my $SMSstring,
    \my $success_string,
    \$provider,
    \$receiver,
    \$sender,
    \$data);

# Open a filehandle for communications with the remote server.
    my $server = $web_server;
    my $port = 80;
    $server = $proxy if ($proxy);
    $port = $proxyport if ($proxyport);
    
    openSocket(
    \my $filehandle,
    \$server,
    \$port );

# Send our data to the remote server.
  sendData(
    \$filehandle,
    \$web_method,
    \$web_methodstring,
    \$web_referer,
    \$web_cookie,
    \$web_server,
    \$SMSstring);

# Check to see if the remote server accepted our data.
  if (!waitforReply(
    \$filehandle,
    \$success_string) )
  {
    printf("Success! Your message should be on it's way.\n");
    # Cleanup: if our data socket is open, close it.  Not necessary in most
    #   cases, but it's good form.
    if (<$filehandle>) { close $filehandle; }
    exit(0);                 ### Return to calling program with a success code.
  }
  else
  {
    # Close filehandle (if still open) and return to calling program with a failure code.
    &exit("FAILURE! $provider replied with an error.",$filehandle);
  }
}


############
# Function: processCmdLine(\$receiver, \$provider, \$sender, \$message, \$proxy)
# Pass by reference the following scalars:
#   -  $receiver is the variable which will store the receiver's cell number.
#   -  $provider is the variable which will store the service provider's name.
#   -  $sender is the variable which will store the sender's name / number.
#   -  $message is the variable which will store the text of the message body.
# Initially these are null scalars, but after parsing the command line,
#   these variables will have the above specified values.  Note that $sender
#   and $message are considered optional (although not specifying $message
#   is silly).
# If invalid command-line parameters or garbage are found, the subroutine will
#   act accordingly, perhaps with just a warning message, or by halting
#   execution with an error message.
# If STDIN contains any info, then it will override data passed via "-m" as the
#   text of the message body.  This can be useful for some applications, or
#   just for script debugging and quick messages.
# One other note: sendSMS.pl will allow the user to send an 'empty' message
#   (that is, one with no sender info or message body).  This is by design.
#   Certain providers will actually allow more data to be passed in the sender
#   field than in the message field, so omitting the message body will actually
#   allow more text to be sent to the recipient.  Also, some providers
#   require both sender & message, while others require only one (and still
#   others require neither).  It has been deemed best to remain flexible, and
#   let the user decide which fields to use, depending on provider.  As a side
#   effect, it is possible to send a 'blank' message with neither sender nor
#   message body, although it would be generally silly to do so.
###
sub processCmdLine
{
  my $numargs = @ARGV;		### Count # of args passed via cmdline
  # Get incoming variables (those passed to this function)
  (
    my $receiver,
    my $provider,
    my $sender,
    my $message,
    my $proxy,
    my $proxyport
    ) = @_;

if ($^O !~ /win/i) {     ### STDIN not supported for Win32
  # Here we try to parse anything in the STDIN queue (for instance, an
  #   echo "blah" | sendSMS.pl -r 111222333 -p BLAH   command)
  use Fcntl;                      ### Need to use Perl's file control semantics.

  # Set STDIN to non-blocking mode (so if there's no input, it won't wait for it)
  fcntl(STDIN, F_SETFL, O_NONBLOCK)
                or die "ERROR: can't set non blocking: $!";
  chomp($$message = <STDIN>);     # Check for STDIN message body.
}

# For loop to process each command-line argument in turn...
for (my $count = 0; $count < $numargs; $count++)
  {
    if ($ARGV[$count] =~ /^-r/ )
    {				### Check for recipient (-r) flag and make sure
      $count++;			### next param isn't another argument flag.
      if ($ARGV[$count] && $ARGV[$count] !~ /^-/ ) {
        $$receiver=$ARGV[$count];
      }
    }
    elsif ($ARGV[$count] =~ /^-p/ )
    {				### Check for provider (-p) flag and make sure
      $count++;			### next param isn't another argument flag.
      if ($ARGV[$count] && $ARGV[$count] !~ /^-/ ) {
        $$provider=$ARGV[$count];
      }
    }
    elsif ($ARGV[$count] =~ /^-s/ )
    {				### Check for sender (-s) flag. The next param
      $count++;			### is any text, so a "-" is OK.
      if ($ARGV[$count] ) {
        $$sender=$ARGV[$count];
      }
    }
    elsif ($ARGV[$count] =~ /^-m/ && !$$message)
    {				### Check for message (-m) flag.  The next
      $count++;			### param is any text, so a "-" is OK.
      if ($ARGV[$count] ) {     ### NOTE that if STDIN contains message data,
        $$message=$ARGV[$count];###then the -m flag will be ignored.
      }
    }
    elsif ($ARGV[$count] =~ /^-x/ )
    {				### Check for proxy (-x) flag. The next param
      $count++;			### is any text, so a "-" is OK.
      if ($ARGV[$count] ) {
        $$proxy=$ARGV[$count];
        if ($$proxy =~ /:/) {
          ($$proxy, $$proxyport) = split(/:/, $$proxy);
        }
      }
    }
    else {			### There's a bad argument somewhere, so
      if($ARGV[$count]=~/^-/) { ### print the summary help screen, and exit
        &printHelp;		### with a (hopefully) useful error message.
        &exit("Invalid command-line parameter: $ARGV[$count]");
      }
      else { 			### If it's unattached command-line trash, then
				### ignore it, warn the user, and keep running.
        print("sendSMS: Warning: Ignored command-line garbage: $ARGV[$count]\n");
      }
    }
  }
}


############
# Function: checkInput(\$provider, \$receiver)
# Pass by reference two scalars:
#   -  $provider is the name of our selected service provider.
#   -  $receiver is the cellular number (10-digit) of our recipient.
#
# This function will simply perform a sanity check on these two values
#   to prevent any stupid errors (either on the programmer's or the user's
#   part) from causing havoc and possibly sending garbage out on the 'net.
###
sub checkInput
{
  (
  my $provider,
  my $receiver
  ) = @_;

### Check input:
  # Make sure destination number and service provider are not null (we will
  # allow null sender and data, however.)  Also be sure destination number
  # is in the correct format.
  if (! $$provider)  # was there no provider selected?
  {
    &printHelp;
    # _some_ cmdline params were passed, so we'll give the user a starting hint:
    if (@ARGV) {
      &exit("HINT: Please specify a service provider code from the above list.");
    }
    # _no_ cmdline params were passed, so maybe they just want to see the standard "help":
    exit(1);
  }
##  # Basic sanity check on the cellular number (must be exactly 10 digits, positive integer).
##  if ($$receiver eq '' || !(length( int($$receiver) ) eq 10) ||
##      ($$receiver < 0 && $$receiver > 9999999999) )
##  {
##    &printHelp; &exit("HINT: Please specify a 10-digit number (ie: 2223334444) for the\n               recipient's phone.");
##  }
}


############
# Function: encodeAsURL(\$string1[,\$string2 .. $stringN] )
# Pass the reference to one or more strings to this function, and it will
#   convert the control characters to properly-formatted URL-encoding.
#   Upon completion, the original string(s) in the caller's context will have
#   been modified.
###
sub encodeAsURL
{
  # A for loop to execute the encode routine for each passed variable:
  for (my $count = 0; $count < @_; $count++)
  {
    my $data = $_[$count];
    # Here's the conversion...
    $$data =~ s/([^a-zA-Z0-9])/'%'.unpack("H*",$1)/eg;
    $$data =~ tr/ /+/;
  }
}


############
# Function: openSocket(\$filehandle, \$servername)
# Pass a reference to two scalars in the caller's context:
#   -  $filehandle is used is a 'pseudo-filehandle' -- that is, it will work
#   anywhere a "real" filehandle would, even though it appears to be a regular
#   scalar variable.  This has some unique advantages, such as being able to
#   pass references to a socket back and forth between functions.  For
#   more info, see http://perl.plover.com/local.html.
#   -  $servername is the name of the remote server which we are going
#   to attempt to connect our socket to.
#   NOTE: Failures at this stage will automatically end the program, exiting
#   with an error message that will hopefully provide some useful info.
###
sub openSocket
{
  # First, we collect the passed references:
  (
    my $socket,
    my $server,
    my $port,
  ) = @_;

# Create the socket.  There is some trickery involved with the second line,
#   in that we create a null glob and then assign it to scalar $$socket.
#   The "do { local *SERVER }" part creates the glob, then destroys it's
#   entry in the naming tables.  However, the data gets tagged with the name of
#   scalar $$socket, which basically means we have a null glob that is
#   referenced as a scalar and can be treated as such -- passed around, etc.
#   For our purposes this is advantageous.
# See the function comments (above) for more info and a web page which
#   explains the process in greater detail.
  use Socket;
  $$socket = do { local *SERVER };

# Open TCP socket in streaming mode:
  socket($$socket, PF_INET, SOCK_STREAM, getprotobyname('tcp')) || &exit("ERROR: couldn't create an outbound streaming TCP socket");

# Create data struct we will be using to make the connection: sockaddr_in(port,IPv4 destination)
  my $stream = sockaddr_in ($$port, inet_aton($$server)) || &exit("DEBUG: couldn't get sockaddr_in for hostname\"$$server\"",$$socket);

# Connect our socket ($$socket) to web server defined as ($stream):
  connect($$socket, $stream) || &exit("ERROR: couldn't connect to host\"$$server\".\n                Perhaps it's down, or doesn't exist?",$$socket);

# Force our output to flush immediately after a print (no buffers).
  select($$socket);
  $| = 1;
  select(STDOUT);
}


###########
# Function: sendData( \$socketFileHandle, \$HTTPRequestMethod,
#   \$HTTPRequestString, \$HTTPReferer, \$HTTPCookie, \$remoteHostName,
#   \$formDataString )
# Pass this function several references to scalars, as follows:
#   -  $socketFileHandle is the scalar name of the filehandle we are
#   printing to.
#   -  $HTTPRequestMethod is either "GET" or "POST", depending
#   upon what is specified in the selected provider's template.
#   -  $HTTPRequestString is the rest of the GET or POST line that
#   is to be sent to the remote server.  Again, part of the template.
#   -  $HTTPReferer is the referer string defined in the provider's template.
#   -  $HTTPCookie is the cookie string defined in the provider's template.
#   Note that the cookie string can be left blank. In fact, it probably should,
#   unless you *know* your provider requires a cookie.
#   -  $remoteHostName is the name of the remote webserver.
#   -  $formDataString is the actual data, formatted differently per provider,
#   that will be submitted to the provider's website.  This string is defined
#   as part of each provider's template.
#
# This function will correctly format the GET/POST request, tack on a
#   bunch of HTTP headers (some static, some based on the passed data),
#   and send the entire string to the server.  Note that not all headers are
#   *required* -- but we generate them anyway, because we want this
#   utility to appear to be a "legit" web client when it hits the server's logs.
###
sub sendData
{
  ## Get incoming variables
  (
    my $remoteHostSocket,
    my $method,
    my $methodstring,
    my $referer,
    my $cookie,
    my $server,
    my $data
  ) = @_;

# If GET is used, then $$data *is* the method string, so overwrite $$methodstring:
  if ($$method eq "GET") { $$methodstring = $$data; }

# Build the HTTP request...
  my $string = "$$method $$methodstring HTTP\/1.1\015\012";
  $string .= "Referer: $$referer\015\012";
  $string .= "Host: $$server\015\012";
  $string .= "Accept-Language: en-us\015\012";
  $string .= "User-Agent: Mozilla\/5.0 \(compatible; Konqueror\/2.0.1; X11\)\015\012";
  $string .= "Connection: Keep-Alive\015\012";
# If a cookie is defined (currently, only SPRINT needs one), then add it:
  if ($$cookie) { $string .= "Cookie: $$cookie\015\012"; }
# If POST is used, then there are some extra headers to be generated:
  if ($$method eq "POST") {
    my $length = (length $$data);
    $string .= "Content-type: application/x-www-form-urlencoded\015\012";
    $string .= "Content-length: $length\015\012";
    $string .= "\015\012$$data";
  }
# Send the entire string to the remote server as one request:
  print $remoteHostSocket ("$string\015\012");
}


############
# Function: waitforReply(\$socketFileHandle, \$successString)
# Pass two scalars by reference, as follows:
#   -  $socketFileHandle is the scalar name of the filehandle we are
#   reading from.
#   -  $successString is the string we are waiting to receive back
#   from the remote server, to signal that we were successful.  This is
#   defined differently for each provider, as part of their template.
#
# This function will read in all data from the remote end, until either
#   (a) the success string is found, or (b) the remote end closes the
#   connection.  Returns 0 if the string was found, 1 if not.
###
sub waitforReply
{
  ## Get incoming data...
  (
    my $returnData,
    my $flag
  ) = @_;

#  Here we listen to the remote web server's reply, and wait until we see our
#  'success' string in the reply [meaning the message was accepted], or until
#  the remote server closes the socket [meaning the message probably wasn't
#  accepted].
  while (<$returnData>) {
    my $string = $_;              # Read data from our open filehandle, until it closes.
    if ($string =~ m/$$flag/i) {  # Waiting to spot the 'success' string...
      return(0);                  # Success!
    }
  }
return(1);                        # Error!
}


############
# Function: printHelp()
# Prints help message to the currently selected output stream, then returns.
###
sub printHelp
{
 print $prognfo;
 print <<EOM;
Usage: sendSMS.pl -r receiver -p provider [-s sender] [-m message] [-x proxy:port]
EOM

if ($^O !~ /win/i) {     ### Win32 support for STDIN is broken
  print"   or: echo <message> | sendSMS.pl -r receiver -p provider [-s sender] [-x proxy:port]\n";
}
 print <<EOM;
  <receiver> is the cellular number of phone which will receive
    the SMS message.  Note: this number MUST be in 10-digit format.
    (i.e. '2223334444')
  <provider> denotes which provider the receiving cellphone is serviced by.
    Currently, the following provider codes are recognized:
EOM

# Generate and print the list of providers:
  my $a; my $tmp;
  # Call to getProvider w/magic value, to get array list of providers returned:
  my @list = getProvider('','','','','get providerNames','','','','','');
  # Here we actually format and print the list...
  for ($a=1; $a < @list; $a+=3) # Read each provider record
  {
    $tmp = "      $list[$a] - $list[$a + 1]";   # Print the short and long name
    printf ("%-41s",$tmp);                      # right-justified 41-char field
      if (@list-3 > $a && length ( $tmp) < 42 ) # If it's not too crowded,
      {                                         # Print a second record too
        $a+=3;                                  # Point to next record
        print "  $list[$a] - $list[$a+1]";
      }
    print "\n";                                 # Newline, loop to read more.
  }
  print <<EOM;
  <sender> may be the sender's name, email, phone, etc (any text string)
    Note that some services may impose additional limits on the contents of
    this field.  Although sendSMS doesn't require a sender, some providers may.
  <message> is the text body of your message.  Since SMS only provides for
    160 characters per message (including headers, etc.), this will probably
    be 127-145 characters, depending upon your provider.

EOM
}


############
# Function: exit($errorMessageString, $filehandle)
# Exit, printing an error message (if applicable), and close open filehandles.
# Parameters:
#   -  $errorMessageString: the text of an extended error message.  It is
#   printed out when the program bails, to give a more detailed explaination
#   of the failure.
#   -  $filehandle: the scalar representation of an open filehandle (glob)
#   which needs to be closed before program execution ends.
# NOTE that this function takes 'normally-passed' variables, not references.
###
sub exit
{
  if (length($_[0])) { printf "sendSMS: $_[0]\n"; }  # Print error message, if passed.
  # Cleanup: if our data socket is open, close it.  Not necessary in most
  #  cases, but it's good form.
  if (<$_[1]>) { close $_[1]; }  # Close open filehandle, if it's still open.
  exit(1);			### Return to calling program w/an error code
}

