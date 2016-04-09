<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>

  <!-- Author: Paul Schaaf -->

  <!-- <xsl:variable name="embedded_table" select="'&lt;table border=&quot;1&quot; cellspacing=&quot;0&quot; cellpadding=&quot;1&quot; width=&quot;50%&quot;&gt;'"/> -->

<!-- Launch in IE
   ieViewLaunch: function (argumentstext) {
     file = Components.classes['@mozilla.org/file/local;1'].createInstance(Components.interfaces.nsILocalFile);
     file.initWithPath("c:\\Program Files\\Internet Explorer\\iexplore.exe");

     var process = Components.classes['@mozilla.org/process/util;1'].getService(Components.interfaces.nsIProcess);
     process.init(file);

     var arguments= [] ;
     arguments.push(argumentstext);

     process.run(false, arguments, arguments.length,{});
   }
-->

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
        <table border="0">
          <tr>
            <td align="right">
              <a href="{url}">
                <img src="{logo}" alt="{long-name}" border="0"/>
              </a>
            </td>
              <td align="left" valign="center">
                <h1>
                  <xsl:apply-templates select="long-name"/>
                </h1>
              </td>
          </tr>
        </table>
        <xsl:apply-templates select="project"/><p/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="project">
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
    <table border="1" cellspacing="0" cellpadding="2" width="100%" nowrap="true">
      <tr bgcolor="lightgreen">
        <th width="1">Name</th>
        <th width="1" align="center">Login</th>
        <th width="1" nowrap="false">Type</th>
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
        <a href="../Login.do?loginName={login}&amp;loginPassword={password}" target="_blank">
           <xsl:apply-templates select="@name"/>
        </a>
      </td>
      <td align="center" nowrap="true">
        <xsl:apply-templates select="login"/>/
        <xsl:apply-templates select="password"/>
      </td>
      <td nowrap="true">
        <xsl:apply-templates select="type"/>
      </td>
      <td>
        <xsl:apply-templates select="role"/>
      </td>
    </tr>
  </xsl:template>

</xsl:stylesheet>
