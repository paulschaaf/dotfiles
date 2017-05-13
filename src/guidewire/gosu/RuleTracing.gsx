package gw
uses java.lang.StringBuffer
uses gw.api.util.Logger

enhancement RuleTracing : com.guidewire.pl.system.gscript.entity.RuleNode {

  @Returns ("The first two characters of the Entity Name.")
  property get EntityType_Ext() : String {
    return this.EntityName.substring( 0, 2 )
  }

  @Returns ("The first word in the rule name (all characters up to but not including the first space).")
  property  get Code_Ext() : String {
    return this.DisplayName.split(" ")[0]

    // Alternative implementation: Return up to the first 8 characters regardless of what they are
    //var myName = this.DisplayName
    //return myName.substring( 0, gw.api.util.Math.min( 8, myName.length ) as int )
  }

  @Returns ("A name composed of this rule's EntityType_Ext, RuleSetType, a hyphen, and the Code_Ext.\
  For example an Exposure Validation Rule whose name begins with the word 'test001' would answer \
  something like ExV-text001")
  property get EncodedName_Ext() : StringBuffer {
    return new StringBuffer( this.EntityType_Ext )
      .append( this.RuleSetType )
      .append( "-" )
      .append( this.Code_Ext )
  }

  @Param ("messages", "Custom messages to be written to the logfile.")
  @Returns ("This rule's EncodedName_Ext with each member of messages concatenated to the end.")
  function logEntryFor_Ext(messages : String[]) : String {
    var buffer = this.EncodedName_Ext
    buffer.append(": ")
    messages.each(\ str -> buffer.append(str))
    return buffer as java.lang.String
  }

  @Param ("messages", "Custom messages to be written to the logfile.")
  @Returns ("The boolean true, allowing this to be called from a rule's condition.")
  function logDebug_Ext(messages : String[]) : boolean {
    Logger.logDebug( this.logEntryFor_Ext(messages) )
    return true
  }

  @Param ("messages", "Custom messages to be written to the logfile.")
  @Returns ("The boolean true, allowing this to be called from a rule's condition.")
  function logError_Ext(messages : String[]) : boolean {
    Logger.logError( this.logEntryFor_Ext(messages) )
    return true
  }

  @Param ("messages", "Custom messages to be written to the logfile.")
  @Returns ("The boolean true, allowing this to be called from a rule's condition.")
  function logInfo_Ext(messages : String[]) : boolean {
    Logger.logInfo( this.logEntryFor_Ext( messages ) )
    return true
  }

  @Param ("messages", "Custom messages to be written to the logfile.")
  @Returns ("The boolean true, allowing this to be called from a rule's condition.")
  function logTrace_Ext(messages : String[]) : boolean {
    Logger.logTrace( this.logEntryFor_Ext(messages) )
    return true
  }

  @Param ("messages", "Custom messages to be written to the logfile.")
  @Returns ("The boolean true, allowing this to be called from a rule's condition.")
  function logWarning_Ext(messages : String[]) : boolean {
    Logger.logWarning( this.logEntryFor_Ext(messages) )
    return true
  }
}
