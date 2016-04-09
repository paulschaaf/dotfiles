<?xml version="1.0" encoding="ISO-8859-1"?>
<!--
-*- compile-command: "../updateIssues" -*-
-->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>

  <!--
      (custom-set-faces
      ;; custom-set-faces was added by Custom - don't edit or cut/paste it!
      ;; Your init file should contain only one such instance.
      '(mode-line ((((type x w32 mac) (class color)) (:background "navy" :foreground "yellow" :box (:line-width -1 :style released-button)))))
      '(nxml-comment-content-face ((t (:foreground "yellow4"))))
      '(nxml-comment-delimiter-face ((t (:inherit nxml-comment-content-face))))
      '(nxml-delimited-data-face ((t (:foreground "lime green"))))
      '(nxml-delimiter-face ((t (:foreground "grey"))))
      '(nxml-element-local-name-face ((t (:inherit nxml-name-face :foreground "medium turquoise"))))
      '(nxml-name-face ((t (:foreground "rosy brown"))))
      '(nxml-tag-slash-face ((t (:inherit nxml-name-face :foreground "grey")))))
  -->

  <!-- <?xml-stylesheet type="text/xsl" href="jiras.xsl"?> -->

  <xsl:variable name="displayFontSize" select="4"/>
  <xsl:variable name="jira"            select="concat($jira-base-url,'/browse')"/>
  <xsl:variable name="jira-base-url"   select="'http://jira:9080/jira'"/>
  <xsl:variable name="logo-height"     select="51"/>
  <xsl:variable name="printFontStyle"  select="'font-size:10; font-family:Arial,Sans-Serif;'"/>
  <xsl:variable name="tableHeaderColor" select="'#9acd32'"/>

  <xsl:template match="/feed">
    <html>
      <head>
        <title>Unresolved Jiras</title>
        <link rel="shortcut icon" href="{$jira-base-url}/images/icons/favicon.ico"/>
        <link rel="icon" type="image/png" href="{$jira-base-url}/images/icons/favicon.png"/>
        <style type="text/css">
          <xsl:value-of select="$printFontStyle"/>
        </style>
      </head>
      <body leftmargin="0" topmargin="0" marginwidth="0.5" marginheight="0.5">
        <table width="100%" border="0" cellspacing="0" cellpadding="0"> 
          <tr>
            <table width="100%" border="0" cellspacing="0" cellpadding="0" background="http://www.guidewire.com/images/top_slug.jpg">
              <tr>
                <td align="left" valign="top"> 
                  <a href="http://www.guidewire.com">
                    <img src="http://www.guidewire.com/images/logo.gif" border="0"/>
                  </a>
                </td>
                <td width="5%"/>
                <td width="100%" align="left" valign="center">
                  <font style="{$printFontStyle}" color="white">
                    <h1 class="formtitle">Assigned to pschaaf</h1>
                  </font>
                </td>
              </tr>
            </table>
          </tr>
          <form name="jirastats" method="GET" action="">
            <tr>
              <input type="submit" id="btnG" name="commit" value="Save"/>
            </tr>              
            <xsl:apply-templates select="name"/>
            <xsl:call-template name="textbox">
              <xsl:with-param name="name" select="notes"/>
              <xsl:with-param name="size" select="675"/>
            </xsl:call-template>
            <!-- <textarea cols="135" rows="5" wrap="physical" style="{$printFontStyle}"/> -->
          </form>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="name">
    <tr>
      <td>
        <xsl:variable name="url">
          <xsl:value-of select="."/>
        </xsl:variable>
        <xsl:apply-templates select="document($url)"/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="comment()">
    <p/>
    <xsl:value-of select="."/>
    <p/>
  </xsl:template>

  <xsl:template name="textbox">
    <xsl:param name="name"/>
    <xsl:param name="size"/>
    <input type="text" name="{$name}" size="{$size}"/>
    <!-- <textarea name="{name}" cols="{$cols}" rows="{$rows}" wrap="physical" style="{$printFontStyle}"/> -->
  </xsl:template>

  <xsl:template match="channel">
    <table border="1" cellpadding="1" cellspacing="0" style="{$printFontStyle}">
      <tr height="0" bgcolor="{$tableHeaderColor}">
        <th><b>Customer Site</b></th>
        <th><b>Updated</b></th>
        <th><b>Created</b></th>
        <th><b>Pr</b></th>
        <th><b>Urg</b></th>
        <th><b>Title</b></th>
        <th><b>Key</b></th>
        <th><b>Status</b></th>
        <th><b>Next Steps</b></th>
      </tr>
      <font size="{$displayFontSize}" style="{$printFontStyle}">
        <xsl:for-each select="item">
          <tr height="0" valign="top">
            <xsl:apply-templates select=".">
              <xsl:sort />
            </xsl:apply-templates>
          </tr>
        </xsl:for-each>
      </font>
    </table>
  </xsl:template>

  <xsl:template match="/feed/rss/channel/item">
    <xsl:variable name="url">
        <xsl:value-of select="concat($jira,'/')"/>
        <xsl:value-of select="concat(key,'?os_username=guest&amp;os_password=guest&amp;decorator=none&amp;view=rss')"/>
    </xsl:variable>

    <xsl:call-template name="customer">
      <xsl:with-param name="url" select="$url"/>
    </xsl:call-template>
    <xsl:apply-templates select="updated"/>
    <xsl:apply-templates select="created"/>
    <xsl:apply-templates select="priority"/>
    <xsl:call-template     name="urgency"/>
    <xsl:apply-templates select="title"/>
    <xsl:apply-templates select="key"/>
    <xsl:apply-templates select="status"/>
    <xsl:call-template     name="next-steps">
      <xsl:with-param name="key" select="key"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="customer">
    <xsl:param name="url"/>
    <td>
      <xsl:variable name="customer">
        <xsl:for-each select="document($url)/rss/channel/item/customfields/customfield">
          <xsl:if test="customfieldname = &quot;Customer Site&quot;">
            <xsl:value-of select="customfieldvalues/customfieldvalue"/>
          </xsl:if>
        </xsl:for-each>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="$customer = &quot;&quot;">
          <xsl:value-of select="reporter"/>
        </xsl:when>
        <xsl:when test="$customer = &quot;N/A&quot;">
          <xsl:value-of select="reporter"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$customer"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
  </xsl:template>

  <xsl:template match="key">
    <xsl:variable name="url" select='.'/>
    <td nowrap="true">
      <a href="{$jira}/{$url}" target="_blank">
        <xsl:value-of select="$url"/>
      </a>
    </td>
  </xsl:template>

  <xsl:template match="status">
    <td>
      <select name="next action" style="{$printFontStyle}">
        <option> </option>
        <option>Call cust</option>
        <option>Follow up</option>
        <option>Research</option>
        <option>Resolved?</option>
        <option>Wait Cust</option>
        <option>Wait Eng</option>
        <option>Wait PM</option>
        <option>Wait QA</option>
      </select>
    </td>
  </xsl:template>

  <xsl:template name="urgency">
    <td>
      <select name="urgency" style="{$printFontStyle}">
        <option> </option>
        <option>1</option>
        <option>2</option>
        <option>3</option>
      </select>
    </td>
  </xsl:template>

  <xsl:template match="priority">
    <td>
      <xsl:variable name="priority" select="."/>
      <xsl:variable name="priority_lc_name">
        <xsl:choose>
          <xsl:when test="$priority = 'Blocker'">
            <xsl:value-of select="'blocker'"/>
          </xsl:when>
          <xsl:when test="$priority = 'Critical'">
            <xsl:value-of select="'critical'"/>
          </xsl:when>
          <xsl:when test="$priority = 'Major'">
            <xsl:value-of select="'major'"/>
          </xsl:when>
          <xsl:when test="$priority = 'Minor'">
            <xsl:value-of select="'minor'"/>
          </xsl:when>
          <xsl:when test="$priority = 'Trivial'">
            <xsl:value-of select="'trivial'"/>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <img src="{$jira-base-url}/images/icons/priority_{$priority_lc_name}.gif"/>
    </td>
  </xsl:template>

  <xsl:template match="updated|created">
    <td>
      <xsl:variable name="date">
        <xsl:value-of select="concat(substring-before(.,'2005'),substring-before(substring-after(.,'2005'), ' -'))"/>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="$date = &quot;&quot;">
          <xsl:value-of select="'?'"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$date"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
  </xsl:template>

  <xsl:template match="title">
    <td>
      <xsl:value-of select="substring-after(.,']')"/>
    </td>
  </xsl:template>

  <xsl:template name="next-steps">
    <xsl:param name="key"/>
    <td width="75">
      <!-- <textarea name="{$key}" cols="52" rows="2" wrap="physical"  style="{$printFontStyle}"/> -->
      <xsl:call-template name="textbox">
        <xsl:with-param name="name" select="$key"/>
        <xsl:with-param name="size" select="104"/>
      </xsl:call-template>

    </td>
  </xsl:template>

  <!-- <xsl:template match="customfield">
       <xsl:if test="customfieldname = &quot;Customer Site&quot;">
       <xsl:value-of select="customfieldvalues/customfieldvalue"/>
       </xsl:if>
       </xsl:template> -->

</xsl:stylesheet>

<!--- 
<rss version="0.92">
  <channel>
    <title></title>
    <link></link>
    <description></description>
    <language></language>

    <item>
      <title></title>
      <link></link>
      <description><![CDATA[]]></description>
      <environment><![CDATA[]]></environment>
      <key></key>
      <summary></summary>
      <type id="4"></type>
      <priority id="3"></priority>
      <status id="1"></status>
      <resolution></resolution>
      <assignee username="pschaaf"></assignee>
      <reporter username="bmoore"></reporter>
      <created></created>
      <updated></updated>
      <customfields>
        <customfield>
          <customfieldname>Spec/MRD Update Needed</customfieldname>
          <customfieldvalue>No</customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Date Closed</customfieldname>
          <customfieldvalue></customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Rules Help?</customfieldname>
          <customfieldvalue>No</customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Affects Doc</customfieldname>
          <customfieldvalue>No</customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Release Note Needed</customfieldname>
          <customfieldvalue>No</customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Customer Site</customfieldname>
          <customfieldvalue></customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Date Resolved</customfieldname>
          <customfieldvalue></customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Re-Open Count</customfieldname>
          <customfieldvalue></customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Doc Section</customfieldname>
          <customfieldvalue>Default</customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Customer's ID</customfieldname>
          <customfieldvalue></customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Doc Changed</customfieldname>
          <customfieldvalue>No</customfieldvalue>
        </customfield>
        <customfield>
          <customfieldname>Release Note Complete</customfieldname>
          <customfieldvalue>No</customfieldvalue>
        </customfield>
      </customfields>
    </item>
</channel>
</rss>

-->