<?xml version="1.0"?>
<xsl:transform version="1.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <xsl:output method="xml"/>

  <!--
      xsl -in config-env/config/elements/nvv_financials.xml -xsl `cygpath -asw ~/lib/xml/pretty-nvv-lv.xsl` | decodeScreenElements >| nvv_financials.html
  -->

  <!--
      tables:
      cellpadding is whitespace margin around cell contents
      cellspacing is whitespace margin around cell
  -->

  <!--
      <a href="http://www.ora.com/kumquats/homecooking/recipes.html#quat5"
         onMouseOver="window.status='A recipe for kumquat soup.';return true">
        <img src="pics/bowl.gif" border="0"/>
      </a>
  -->

  <xsl:include href="passthrough.xsl"/>

  <xsl:variable name="child-indent" select="15"/>

  <xsl:template match="/">
    <html>
      <head>
      </head>
      <body>
        <xsl:call-template name="show-children"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="screens|ElementConfig">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="screen">
    <xsl:apply-templates select="document(.)"/>
  </xsl:template>

  <xsl:template match="*">
    <tr>
      <td>
        <xsl:call-template name="print-name"/>
        <xsl:call-template name="show-children-indented-box"/>
        <br/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template name="show-children-indented-box">
    <xsl:if test="count(@*) > 1 or count(*) > 0">
      <table border="0" bordercolor="red" cellpadding="0" cellspacing="0">
        <tr>
          <td width="{$child-indent}"/>
          <td>
            <xsl:call-template name="show-children-boxed"/>
          </td>
        </tr>
      </table>
    </xsl:if>
  </xsl:template>

  <xsl:template name="show-children-boxed">
    <xsl:call-template name="show-attributes"/>
    <xsl:if test="count(*) > 0">
      <table border="1" bordercolor="blue" cellpadding="0" cellspacing="0">
        <tr>
          <td>
            <xsl:call-template name="show-children"/>
          </td>
        </tr>
      </table>
    </xsl:if>
  </xsl:template>

  <xsl:template name="show-children">
    <xsl:if test="count(*) > 0">
      <!-- establish vertical spacing for each child -->
      <table border="0" bordercolor="green" cellpadding="0" cellspacing="0">
        <xsl:call-template name="maybe-show-text"/>
        <xsl:apply-templates/>
      </table>
    </xsl:if>
  </xsl:template>

  <xsl:template name="show-attributes">
    <xsl:if test="@*">
      <table border="0" cellspacing="0" cellpadding="0" style="font-size:90%">
        <xsl:apply-templates select="@*"/>
      </table>
    </xsl:if>
  </xsl:template>

  <xsl:template name="print-name">
    <!-- put the name in a box -->
    <table border="0" bordercolor="yellow" cellpadding="0" cellspacing="2" width="100%">
      <tr>
        <xsl:choose>
          <xsl:when test="name()='NameValueViewDef'">
            <td>
              <h1>
                <xsl:value-of select="@instance"/>
              </h1>
            </td>
          </xsl:when>
          <xsl:when test="name()='SubViewDef'">
            <td>
              <h1>
                <a name="{@instance}">
                  <xsl:value-of select="@instance"/>
                </a>
              </h1>
            </td>
          </xsl:when>
          <xsl:when test="name()='SubView'">
            <td>
              <a href="#{@name}">
                <xsl:value-of select="@name"/>
              </a>
            </td>
          </xsl:when>
          <xsl:when test="name()='Spacer'">
            <td>
              <h1>
                <xsl:value-of select="@name"/>
              </h1>
            </td>
          </xsl:when>
          <xsl:otherwise>
            <xsl:if test="@name">
              <td>
                "<b>
                <a href=""
                   title="display.properties: {@name}">
                  <xsl:value-of select="concat('#{displayKeys[&quot;',@name,'&quot;]}')"/>
                </a>
                </b>"
              </td>
            </xsl:if>
            <td>
              <xsl:choose>
                <xsl:when test="name()='Label'">
                  <h2>
                    <xsl:value-of select="@name"/>
                  </h2>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@name"/>
                </xsl:otherwise>
              </xsl:choose>
            </td>
          </xsl:otherwise>
        </xsl:choose>
        <td>
          <small>
            <xsl:value-of select="concat('&lt;',name(),'&gt;')"/>
          </small>
        </td>
      </tr>
    </table>
  </xsl:template>

  <!--   <xsl:template match="Override/text()"> -->
  <!--     <xsl:apply-templates/> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="text()"/> -->

  <xsl:template name="maybe-show-text">
    <xsl:choose>
      <xsl:when test="name()='Override'">
        <xsl:value-of select="text()"/>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- These are handled specially -->
  <xsl:template match="NameValueViewDef/@instance|SubViewDef/@instance|@name"/>

  <xsl:template match="@value">
    <td nowrap="true">
      <li><xsl:value-of select="name()"/></li>
    </td>
      <td width="5"/>
    <td>
      <b>
        <xsl:value-of select="."/>
      </b>
    </td>
  </xsl:template>

  <xsl:template match="NameValueViewDef/@name|SubViewDef/@name|@*">
    <tr>
      <td nowrap="true">
        <li><xsl:value-of select="name()"/></li>
      </td>
      <td width="5"/>
      <td>
        <xsl:value-of select="."/>
      </td>
    </tr>
  </xsl:template>

  <!--
      Common Elements
  -->

  <!--
      List Views
  -->

  <!--
      <xsl:template match="ListViewDef">
      </xsl:template>
  -->
  <!--   <xsl:template match="ListColumn"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="VirtualColumn"> -->
  <!--   </xsl:template> -->

  <!--
      Name-Value Views
  -->
  <!--   <xsl:template match="CustomTag"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="DateItem"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="ElementConfig"> -->
  <!--   </xsl:template> -->

  <!-- <xsl:template match="Item"> -->
  <!-- </xsl:template> -->

  <!--   <xsl:template match="Label"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="NameValueViewDef"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="Spacer"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="SubView"> -->
  <!--   </xsl:template> -->

  <!--   <xsl:template match="SubViewDef"> -->
  <!--   </xsl:template> -->

</xsl:transform>
