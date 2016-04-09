<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="xml"/>

  <xsl:template match="/">
    <html>
      <xsl:call-template name="html-head"/>
      <xsl:call-template name="html-body"/>
    </html>
  </xsl:template>

  <xsl:template name="html-body">
    <body>
      <h1>XPath view of your document</h1>
      <p>The structure of your document (as defined by 
      the XPath standard) is outlined below.</p>
      <table cellspacing="5" cellpadding="2" border="0">
        <tr>
          <td colspan="7">
            <b>Node types:</b>
          </td>
        </tr>
        <tr>
          <td bgcolor="#99CCCC"><b>root</b></td>
          <td bgcolor="#CCCC99"><b>element</b></td>
          <td bgcolor="#FFFF99"><b>attribute</b></td>
          <td bgcolor="#FFCC99"><b>text</b></td>
          <td bgcolor="#CCCCFF"><b>comment</b></td>
          <td bgcolor="#99FF99"><b>processing instruction</b></td>
          <td bgcolor="#CC99CC"><b>namespace</b></td>
        </tr>
      </table>
      <br/>
      <table width="100%" border="1" bgcolor="#99CCCC" cellspacing="2">
        <tr bgcolor="#99CCCC">
          <td colspan="2">
            <b>root:</b>
          </td>
        </tr>
        <xsl:for-each select="namespace::*">
          <tr bgcolor="#CC99CC">
            <td width="15">   </td>
            <td>
              <xsl:call-template name="namespace-node"/>
            </td>
          </tr>
        </xsl:for-each>
        <xsl:for-each select="*|comment()|processing-instruction()|text()">
          <tr bgcolor="#99CCCC">
            <td width="15">   </td>
            <td>
              <xsl:apply-templates select="."/>
            </td>
          </tr>
        </xsl:for-each>
      </table>
    </body>
  </xsl:template>

  <xsl:template name="html-head">
    <head>
      <title>XPath view of your document</title>
      <style type="text/css">
        <xsl:comment>
          span.literal         { font-family: Courier, monospace; }
        </xsl:comment>
      </style>
    </head>
  </xsl:template>

  <xsl:template match="comment()">
    <table width="100%" cellspacing="2">
      <tr>
        <td bgcolor="#CCCCFF">
          <b>comment: </b>
          <span class="literal">
            <xsl:value-of select="."/>
          </span>
        </td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="processing-instruction()">
    <table border="0" width="100%" cellspacing="2">
      <tr>
        <td bgcolor="#99FF99">
          <b>processing instruction: </b>
          <span class="literal">
            <xsl:text>&lt;?</xsl:text>
            <xsl:value-of select="name()"/>
            <xsl:text>?&gt;</xsl:text>
            <br/>
            <xsl:value-of select="."/>
          </span>
        </td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:if test="string-length(normalize-space(.))">
      <tr>
        <td bgcolor="#CCCC99" width="15">    </td>
        <td bgcolor="#FFCC99" width="100%">
          <b>text: </b>
          <span class="literal">
            <xsl:value-of select="."/>
          </span>
        </td>
      </tr>
    </xsl:if>
  </xsl:template>

  <xsl:template name="namespace-node">
    <table border="0" width="100%" cellspacing="2">
      <tr>
        <td bgcolor="#CC99CC">
          <b>namespace: </b>
          <span class="literal">
            <xsl:value-of select="name()"/>
          </span>
          <br/>
          <span class="literal">
            <xsl:value-of select="."/>
          </span>
        </td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="*">
    <table border="1" width="100%" cellspacing="2">
      <xsl:choose>
        <xsl:when test="count(@*) &gt; 0">
          <tr>
            <td bgcolor="#CCCC99" colspan="2">
              <b>element: </b>
              <span class="literal">
                <xsl:text>&lt;</xsl:text>
                <xsl:value-of select="name()"/>
                <xsl:text>&gt;</xsl:text>
              </span>
              <table border="0" width="100%" cellspacing="2">
                <tr> 
                  <td bgcolor="#CCCC99" width="15">   </td>
                  <td bgcolor="#FFFF99" width="20%">
                    <b>attribute name</b>
                  </td>
                  <td bgcolor="#FFFF99">
                    <b>value</b>
                  </td>
                </tr>
                <xsl:for-each select="@*">
                  <tr>
                    <td bgcolor="#CCCC99" width="15">   </td>
                    <td bgcolor="#FFFF99" width="20%">
                      <span class="literal">
                        <xsl:value-of select="name()"/>
                      </span>
                    </td>
                    <td bgcolor="#FFFF99">
                      <span class="literal">
                        <xsl:value-of select="."/>
                      </span>
                    </td>
                  </tr>
                </xsl:for-each>
              </table>
            </td>
          </tr>
        </xsl:when>
        <xsl:otherwise>
          <tr>
            <td bgcolor="#CCCC99" colspan="2">
              <b>element: </b>
              <span class="literal">
                <xsl:text>&lt;</xsl:text>
                <xsl:value-of select="name()"/>
                <xsl:text>&gt;</xsl:text>
              </span>
            </td>
          </tr>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:for-each select="namespace::*">
        <tr>
          <td bgcolor="#CCCC99" width="15">    </td>
          <td bgcolor="#CC99CC">
            <xsl:call-template name="namespace-node"/>
          </td>
        </tr>
      </xsl:for-each>
      <xsl:for-each select="*|comment()|processing-instruction()|text()">
        <tr bgcolor="#CCCC99">
          <td width="15">   </td>
          <td>
            <xsl:apply-templates select="."/>
          </td>
        </tr>
      </xsl:for-each>
    </table>
  </xsl:template>

</xsl:stylesheet>