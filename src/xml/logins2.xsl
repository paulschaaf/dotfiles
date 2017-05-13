<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>


  <xsl:variable name="project-indent" select="'2%'"/>

  <xsl:template match="customer">
    <html>
      <head>
        <title>
          <xsl:value-of select="concat(@name,' Projects')"/>
        </title>
        <link rel="SHORTCUT ICON" href="images/favicon.ico"/>
      </head>
      <body>
        <h1>
          <table border="0">
            <tr>
              <td align="right">
                <a href="{url}">
                  <img src="{logo}" alt="{long-name}" border="0"/>
                </a>
              </td>
              <td align="left" valign="center">
                <xsl:apply-templates select="long-name"/>
              </td>
            </tr>
          </table>
        </h1>
        <xsl:apply-templates select="project"/><p/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="project">
    <hr/>
    <h2>Project: <a href="../{url}"><xsl:value-of select="@name"/></a></h2>
    <table border="0" cellspacing="0" cellpadding="2" nowrap="true" width="100%">
      <tr>
        <td width="{$project-indent}"/>    <!-- empty cell provides indentation -->
        <td width="100%">
          <table border="0" cellspacing="0" cellpadding="2" nowrap="true" valign="top" width="100%">
            <tr>
              <th align="right" nowrap="true" width="1">Product:</th>
              <td width="100%"><xsl:apply-templates select="product"/></td>
            </tr>
            <tr>
              <th align="right" nowrap="true">Sources:</th>
              <td><xsl:apply-templates select="sources"/></td>
            </tr>
            <p/>
            <tr>
              <th align="right" valign="top">Jump to screen:</th>
              <td><xsl:apply-templates select="screens"/></td>
            </tr>
            <p/>
            <tr>
              <th align="right" valign="top" nowrap="true">Login as:</th>
              <td><xsl:apply-templates select="users"/></td>
            </tr>
          </table>
        </td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="product">
    <a href="{url}"><xsl:value-of select="concat(concat(@name,' '),version)"/></a>
  </xsl:template>

  <xsl:template match="sources|url">
    <a href="{.}"><xsl:value-of select="."/></a>
  </xsl:template>

  <xsl:template match="screens">
    <table border="1" cellspacing="0" cellpadding="1" width="100%" nowrap="true">
      <tr bgcolor="lightgreen">
        <th width="1">Name</th>
        <th nowrap="false">Description</th>
      </tr>
      <xsl:apply-templates select="screen"/>
    </table>
  </xsl:template>

  <xsl:template match="screen">
    <tr>
      <td nowrap="true">
        <a href="../{url}"><xsl:apply-templates select="@name"/></a>
      </td>
      <td>
        <xsl:apply-templates select="description"/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="users">
    <table border="1" cellspacing="0" cellpadding="1" width="100%" nowrap="true">
      <tr bgcolor="lightgreen">
        <th width="1">Name</th>
        <th width="1" align="center">Login</th>
        <th width="100%" nowrap="false">Role</th>
      </tr>
      <xsl:apply-templates select="user">
        <xsl:sort select="@name"/>
      </xsl:apply-templates>
    </table>
  </xsl:template>

  <xsl:template match="user">
    <tr>
      <td nowrap="true">
        <a href="../Login.do?loginName={login}&amp;loginPassword={password}" > 
           <xsl:apply-templates select="@name"/>
        </a>
      </td>
      <td align="center" nowrap="true">
        <xsl:apply-templates select="login"/>/<xsl:apply-templates select="password"/>
      </td>
      <td>
        <xsl:apply-templates select="role"/>
      </td>
    </tr>
  </xsl:template>

</xsl:stylesheet>