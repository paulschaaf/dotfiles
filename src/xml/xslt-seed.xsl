<?xml version="1.0"?>
<!DOCTYPE config [
   <!ENTITY nbsp "&#160;">
]>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

  <xsl:output method="html"/>

  <xsl:variable name="newline">
    <xsl:text>
    </xsl:text>
  </xsl:variable>


  <xsl:variable name="css">
    body {
       font-family: sans-serif;
       leftmargin:  0; topmargin:    0;
       marginwidth: 0; marginheight: 0;
    }
    
    tiny {
       font-size: x-small;
    }
  </xsl:variable>


  <!-- ====================================================================
       ====================================================================
       XSL Templates
  -->
  <xsl:template match="/">
    <html>
      <head>
        <title></title>
        <style type="text/css">
          <xsl:value-of select="$css"/>
        </style>
      </head>
  
      <body>
        <table border="1">
          <xsl:copy>
            <xsl:apply-templates/>
          </xsl:copy>
        </table>
      </body>
    </html>
  </xsl:template>


  <xsl:template match="*">
    <tr>
      <td>
        <xsl:element name="{name()}">
          <xsl:value-of select="name()"/>
          <xsl:apply-templates select="@*"/>
          <br/>
          <xsl:if test="count(*) > 1">
            <table border="1" cellpadding="5">
              <xsl:apply-templates/>
            </table>
          </xsl:if>
        </xsl:element>
      </td>
    </tr>
  </xsl:template>


  <xsl:template match="@*">
    <br/>
    <tiny>
      <xsl:value-of select="concat('@',name(),'=&quot;',.,'&quot;')"/>
    </tiny>
  </xsl:template>


</xsl:stylesheet>

<!--
  <xsl:template match="/">
    <xsl:element name="xsl:stylesheet">
      <xsl:apply-templates select="document('')/xsl:stylesheet/@*"/>
      <xsl:apply-templates select="document('')//xsl:output"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="*">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template name="namespace">
    <xsl:attribute name="{concat('xmlns:',name())}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="@*">
    <xsl:attribute name="{name()}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="comment()">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="processing-instruction()">
    <xsl:processing-instruction name="{name()}">
      <xsl:value-of select="."/>
    </xsl:processing-instruction>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>
-->

