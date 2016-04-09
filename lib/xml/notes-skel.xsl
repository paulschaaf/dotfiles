<?xml version="1.0"?>
<!-- <!DOCTYPE message SYSTEM "" [
<!ENTITY nbsp "&#160;">
]> -->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml"/>

  <xsl:variable name="newline">
    <xsl:text>
    </xsl:text>
  </xsl:variable>

  <!-- Print nodes unchanged -->
  <xsl:template match="key">
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="/">
    <items>
      <xsl:apply-templates/>
    </items>
  </xsl:template>

  <xsl:template match="item|status">
    <xsl:element name="{name()}">
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="@*">
    <xsl:attribute name="{name()}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <!-- <xsl:template match="comment()">
         <xsl:comment>
           <xsl:value-of select="."/>
         </xsl:comment>
         <xsl:value-of select="$newline"/>
       </xsl:template>
       
       <xsl:template match="processing-instruction()">
         <xsl:processing-instruction name="{name()}">
           <xsl:value-of select="."/>
         </xsl:processing-instruction>
       </xsl:template> -->

  <!-- Remove these elements -->
  <!-- <xsl:template match="status"/> -->

  <!-- Lookup customer name -->
  <!-- <xsl:template name="customer">
         <xsl:element name="customer">
           <xsl:value-of select="'[customer name]'"/>
         </xsl:element>
       </xsl:template> -->

</xsl:stylesheet>
