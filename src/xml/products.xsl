<?xml version="1.0"?>
  <!-- <!DOCTYPE message SYSTEM "" [
         <!ENTITY nbsp "&#160;">
       ]> -->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>

  <xsl:variable name="nbsp">&#160;</xsl:variable>

  <xsl:template match="products">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="product">
    <xsl:apply-templates select="version"/>
  </xsl:template>

  <xsl:template match="version">
    <b>
      <xsl:value-of select="concat(../name,' ')"/>
      <a href="{url}">
        <xsl:value-of select="@number"/>
      </a>
    </b>
    <table border="1" cellspacing="0" cellpadding="1" width="75%" nowrap="true">
      <tr bgcolor="lightgreen">
        <th width="20%">Name</th>
        <th nowrap="false">Description</th>
      </tr>
      <xsl:apply-templates select="has-screen"/>
    </table>
    <br/>
  </xsl:template>

  <xsl:template match="has-screen">
    <xsl:variable name="id" select="."/>
    <xsl:for-each select="../../screen">
      <xsl:if test="@id = $id">
        <tr>
          <xsl:call-template name="screen"/>
        </tr>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="screen">
    <xsl:for-each select="name|description">
      <td nowrap="true">
        <xsl:apply-templates select="."/>
      </td>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="name">
    <a href="../{../url}"><xsl:apply-templates/></a>
  </xsl:template>

  <xsl:template match="description">
    <xsl:choose>
      <xsl:when test=". = &quot;&quot;">
        <xsl:value-of select="$nbsp"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

