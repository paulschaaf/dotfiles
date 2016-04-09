<?xml version="1.0"?>
<!-- <!DOCTYPE xsl:stylesheet SYSTEM "" [
<!ENTITY nbsp "&#160;">
]>
-->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

  <xsl:output method="xml"/>

  <xsl:variable name="newline">
    <xsl:text>
    </xsl:text>
  </xsl:variable>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

  <!-- Print nodes unchanged -->
  <xsl:template match="*">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- <xsl:for-each select="namespace::*"> -->
  <!--   <xsl:call-template name="namespace"/> -->
  <!-- </xsl:for-each> -->

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

</xsl:stylesheet>

