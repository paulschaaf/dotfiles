<?xml version="1.0"?>
<!DOCTYPE config [
   <!ENTITY attributes-ignore "
         @active
       | @child-group-id
       | @group-id
       | @id
       | @length
       | @public-id
       | @stop-execution
   ">

   <!ENTITY elements-ignore "
         tns:export
       | tns:filtered
       | tns:libraries
       | tns:name
       | tns:res-ctx
       | tns:script-parameters
   ">
]>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tns="http://guidewire.com/xml"
                xmlns:xsd="http://www.w3.org/1999/XMLSchema"
                xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">


  <xsl:output method="html"/>

  <xsl:variable name="debug" select="false()"/>
  <xsl:variable name="title" select="PGS Rule Repository Report"/>

  <!-- ==================================
       XSL search indexes
  -->
  <xsl:key name="rules-with-type"
           match="tns:rule-sets/tns:RuleTreeExport/tns:rule"
           use="@rule-set-type"/>


  <!-- ====================================================================
       ====================================================================
       XSL Templates
  -->
  <xsl:template match="&attributes-ignore;|&elements-ignore;"/>


  <xsl:template match="/">
    <html>
      <head>
        <title><xsl:value-of select="$title"/></title>
        <style type="text/css">
          <xsl:value-of select="$css"/>
        </style>
      </head>
  
      <body>
        <title><xsl:value-of select="$title"/></title>
        <h1>Rule Repository Report</h1>
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>


  <xsl:template match="tns:rule-sets|tns:children">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@*"/>
      <table border="1" bordercolor="red" cellspacing="0">
        <xsl:for-each select="*">
          <tr>
            <td>
              <xsl:apply-templates select="."/>
            </td>
          </tr>
        </xsl:for-each>
      </table>
    </xsl:element>
  </xsl:template>


  <xsl:template match="tns:RuleTreeExport">
    <xsl:copy>
      <xsl:apply-templates select="tns:rule"/>
      <xsl:apply-templates select="tns:children"/>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="tns:___rule-sets/tns:RuleTreeExport">
    <!-- A ruletreeexport contains a rule and its children -->
    <!-- Muench method for grouping
         To iterate through the rules-with-type:
         for each rule
         use key() to retrieve all rules with the same rule-set-type
         generate an id for the first node and the current node
         if they're the same, then the nodes are the same
    -->
    <xsl:for-each select="tns:rule[ generate-id() = 
                          generate-id(key('rules-with-type', @rule-set-type)[1])
                          ]">
      <hr/>
      <xsl:copy>
        <xsl:apply-templates select="@rule-set-type"/>
        <xsl:for-each select="key('rules-with-type', @rule-set-type)">
          <xsl:apply-templates select="."/>
        </xsl:for-each>
      </xsl:copy>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="tns:rule">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@name"/>
      <xsl:apply-templates select="@rule-set-type"/>
      <xsl:apply-templates select="@rule-set-desc"/>
      <br/>
      <xsl:if test="count(*) > 1">
        <xsl:apply-templates/>
      </xsl:if>
    </xsl:element>
  </xsl:template>


  <xsl:template match="tns:version">
    <xsl:copy>
      version <xsl:value-of select="."/>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="*">
    <xsl:element name="{name()}">
      <xsl:if test="$debug = true()">
        <xsl:value-of select="name()"/><br/>
      </xsl:if>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <!-- ====================================================================
       ====================================================================
       Common Attributes
  -->
  
  <!-- <xsl:template match="@entity-name">
  ....   <xsl:value-of select="concat(' (', ., ')')"/>
  .... </xsl:template> -->


  <xsl:template match="@name">
    <xsl:value-of select="."/>
  </xsl:template>


  <xsl:template match="@rule-set-desc">
    <br/>
    <rule-set-desc>
      <xsl:value-of select="."/>
    </rule-set-desc>
  </xsl:template>


  <xsl:template match="@rule-set-type">
    <xsl:variable name="ruleset">
      <xsl:choose>
        <xsl:when test=". = 'A'">Assignment</xsl:when>
        <xsl:when test=". = 'AP'">Approval Routing</xsl:when>
        <xsl:when test=". = 'C'">Closed</xsl:when>
        <xsl:when test=". = 'E'">Excalation</xsl:when>
        <xsl:when test=". = 'EM'">Event</xsl:when>
        <xsl:when test=". = 'IR'">Initial Reserve</xsl:when>
        <xsl:when test=". = 'L'">Loaded</xsl:when>
        <xsl:when test=". = 'PO'">Post-Setup</xsl:when>
        <xsl:when test=". = 'PR'">Pre-Setup</xsl:when>
        <xsl:when test=". = 'PU'">Pre-Update</xsl:when>
        <xsl:when test=". = 'RO'">Reopened</xsl:when>
        <xsl:when test=". = 'S'">Segmentation</xsl:when>
        <xsl:when test=". = 'TAR'">Transaction Approval</xsl:when>
        <xsl:when test=". = 'V'">Validation</xsl:when>
        <xsl:when test=". = 'WP'">Workplan</xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('## Unmapped: ', .)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:attribute name="{name()}">
      <xsl:value-of select="concat($ruleset, ' Rule Sets')"/>
    </xsl:attribute>
  </xsl:template>


  <xsl:template match="@*">
    <br/>
    <tiny>
      <xsl:value-of select="concat('@',name(),'=&quot;',.,'&quot;')"/>
    </tiny>
  </xsl:template>


  <xsl:variable name="css">
    @namespace tns url(http://guidewire.com/xml);

    body {
       font-family: sans-serif;
       leftmargin:  0; topmargin:    0;
       marginwidth: 0; marginheight: 0;
    }
    
    h1 {
       font-weight:  bold;
       text-align:   center;
    }
    
    action, condition {
    }

    children {
       <!-- margin-left: 25pt; -->
    }

    rule-set-name {
       margin:     0%;
       font-size:  x-large;
    }

    rule {
       font-size: larger;
    }
    
    RuleTreeExport {
       <!-- margin-left: 25pt; -->
    }

    rule-set-desc {
       font-size: x-small;
    }

    tiny {
       font-size: x-small;
    }

    version {
       font-size: small;
    }
  </xsl:variable>

</xsl:stylesheet>

  <!-- ====================================================================
       ====================================================================
       DISPLAY OPTIONS: These can be adjusted
  -->

  <!-- 
       * tr.stats-header {
       a:active {
       a:hover {
       a:hover.nolink {
       a:link {
       a:visited {
       align: right;
       background-color: lightgray;
       border-bottom: hidden;
       border-collapse: collapse;
       border-left: 1px solid gray;
       border: 1px dashed gray;
       cellpadding: 100;
       cellspacing: 100;
       color: gray;
       empty-cells: show;
       font-family: Arial,Sans-Serif;
       font-size: 80%;
       font-size: 11px;
       font-weight: bold;
       leftmargin:  0;
       marginheight: 0;
       marginwidth: 0;
       table {
       table-layout: auto;
       table.items {
       table.stats {
       td.Alert {
       text-align: right;
       text-decoration: line-through;
       text-decoration: none;
       text-decoration: underline;
       text-indent: 1em;
       th.total {
       topmargin:    0;
       tr.item-header {
       tr:first-child.stats {
       vertical-align: top;
       width: auto;
  -->
<!--
-->
  <!-- ====================================================================
       ====================================================================
       PARAMETERS: Do not change indiscriminately!
  -->

  <!-- <xsl:variable name="newline">
  ....   <xsl:text>
  ....   </xsl:text>
  .... </xsl:variable> -->


<!--
* tr.stats-header {
a:active {
a:hover {
a:hover.nolink {
a:link {
a:visited {
align: right;
background-color: lightgray;
border-bottom: hidden;
border-collapse: collapse;
border-left: 1px solid gray;
border: 1px dashed gray;
cellpadding: 100;
cellspacing: 100;
color: gray;
empty-cells: show;
font-family: Arial,Sans-Serif;
font-size: 80%;
font-size: 11px;
font-weight: bold;
leftmargin:  0;
marginheight: 0;
marginwidth: 0;
table {
table-layout: auto;
table.items {
table.stats {
td.Alert {
text-align: right;
text-decoration: line-through;
text-decoration: none;
text-decoration: underline;
text-indent: 1em;
th.total {
topmargin:    0;
tr.item-header {
tr:first-child.stats {
vertical-align: top;
width: auto;
-->
