<?xml version="1.0"?>
<!DOCTYPE config [
  <!ENTITY as-rss    "&amp;decorator=none&amp;view=rss">
  <!ENTITY show-unmapped-custom-fields         "true()">
]
>

<xsl:transform version="1.0"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml"/>

  <!-- <xsl:variable name="customers" select="'AMERCO Canal CIGA EDIC MSA NewJerseyMfgGrp'"/> -->

  <xsl:variable name="display-stylesheet" select="'owner-report.xsl'"/>
  <xsl:variable name="jira-base-url"   select="'http://jira/jira'"/>
  <xsl:variable name="jira"            select="concat($jira-base-url,'/browse')"/>
  <xsl:variable name="newline">
    <xsl:text>
    </xsl:text>
  </xsl:variable>


  <xsl:template match="/">
    <!-- <xsl:comment>-*- eval: (indent-region (point-min) (point-max) nil) -*-</xsl:comment> -->
    <xsl:value-of select="concat($newline,$newline)"/>
    <xsl:processing-instruction name="xml-stylesheet">
      <xsl:text>type="text/xsl" href="</xsl:text>
      <xsl:value-of select="$display-stylesheet"/>
      <xsl:text>"</xsl:text>
    </xsl:processing-instruction>
    <xsl:value-of select="concat($newline,$newline)"/>
    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="feed">
    <xsl:copy>
      <xsl:apply-templates select="document(&quot;feed.xml&quot;)/feed/task"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="url">
    <xsl:copy>
      <xsl:value-of select="."/>
    </xsl:copy>
    <!-- <xsl:comment>
    ....   <xsl:copy-of select="."/>
    .... </xsl:comment> -->
    <xsl:apply-templates select="document(concat(.,'&amp;os_username=guest&amp;os_password=guest&as-rss;'))/rss/channel/item"/>
      <!-- <xsl:sort select="customer"/>
      .... <xsl:sort select="priority"/>
      .... <xsl:sort select="key"/> -->

      <!-- Individually fetch all values for each item -->
      <!-- <xsl:for-each select="document(concat(.,'&amp;os_username=guest&amp;os_password=guest&amp;decorator=none&amp;view=rss'))/rss/channel/item/link">
      ....   <xsl:variable name="url">
      ....   <xsl:value-of select="."/>?os_username=guest&amp;os_password=guest&amp;decorator=none&amp;view=rss</xsl:variable>
      ....   <xsl:apply-templates select="document($url)/rss/channel/item">
      ....     <xsl:sort select="customer"/>
      ....   </xsl:apply-templates>
      ....   <xsl:value-of select="$newline"/>
      .... </xsl:for-each> -->
  </xsl:template>


  <xsl:template match="item">
    <item>
      <xsl:attribute name="key">
        <xsl:value-of select="key"/>
      </xsl:attribute>
      <xsl:apply-templates/>
      <!-- <description>
      ....   <xsl:value-of select="description"/>
      .... </description> -->
    </item>
    <xsl:value-of select="$newline"/>
  </xsl:template>


  <!-- <xsl:template match="item/description"/> -->


  <xsl:template match="comments">
    <xsl:apply-templates select="comment[last()]"/>
  </xsl:template>


  <xsl:template match="link"/>


  <xsl:template match="key">
    <xsl:variable name="this-key" select="."/>

    <xsl:element name="{name()}">
      <xsl:apply-templates/>
    </xsl:element>
    <xsl:value-of select="$newline"/>

    <xsl:for-each select="document(&quot;feed.xml&quot;)/feed/item">
      <!-- but what if key is not found? -->
      <xsl:if test="key = $this-key">
        <xsl:call-template name="support-action"/>
        <xsl:apply-templates select="action-detail"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>


  <xsl:template name="value-or-if-empty">
    <xsl:param name="value"/>
    <xsl:param name="default"/>
    <xsl:choose>
      <xsl:when test="string($value) = &quot;&quot;">
        <xsl:value-of select="$default"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$value"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="priority">
    <xsl:variable name="priority_number">
      <xsl:choose>
        <xsl:when test=". = 'Blocker'">
          <xsl:value-of select="1"/>
        </xsl:when>
        <xsl:when test=". = 'Critical'">
          <xsl:value-of select="2"/>
        </xsl:when>
        <xsl:when test=". = 'Major'">
          <xsl:value-of select="3"/>
        </xsl:when>
        <xsl:when test=". = 'Minor'">
          <xsl:value-of select="4"/>
        </xsl:when>
        <xsl:when test=". = 'Trivial'">
          <xsl:value-of select="5"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>


    <xsl:element name="priority">
      <xsl:attribute name="num">
        <xsl:value-of select="$priority_number"/>
      </xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <xsl:template name="support-action">
    <xsl:element name="support-action">
      <xsl:call-template name="value-or-if-empty">
        <xsl:with-param name="value" select="support-action"/>
        <xsl:with-param name="default" select="' '"/>
      </xsl:call-template>
    </xsl:element>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="customfields">
    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="customfield">
    <xsl:choose>
      <xsl:when test="customfieldname = &quot;Action&quot;">
        <xsl:call-template name="action">
          <xsl:with-param name="value" select="customfieldvalues/customfieldvalue"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="customfieldname = &quot;Customer's ID&quot;">
        <xsl:call-template name="customers-id">
          <xsl:with-param name="value" select="customfieldvalues/customfieldvalue"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="customfieldname = &quot;Customer Site&quot;">
        <xsl:call-template name="customer">
          <xsl:with-param name="value" select="customfieldvalues/customfieldvalue"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="customfieldname = &quot;Current Status&quot;">
        <xsl:call-template name="current-status">
          <xsl:with-param name="value" select="customfieldvalues/customfieldvalue"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="customfieldname = &quot;Urgency&quot;">
        <xsl:call-template name="urgency">
          <xsl:with-param name="value" select="customfieldvalues/customfieldvalue"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="customfieldname = &quot;Affects Doc&quot;"/>
      <xsl:when test="customfieldname = &quot;Doc Changed&quot;"/>
      <xsl:when test="customfieldname = &quot;Doc Section&quot;"/>
      <xsl:when test="customfieldname = &quot;Participants&quot;"/>
      <xsl:when test="customfieldname = &quot;Product&quot;"/>
      <xsl:when test="customfieldname = &quot;Release Note Complete&quot;"/>
      <xsl:when test="customfieldname = &quot;Release Note Needed&quot;"/>
      <xsl:when test="customfieldname = &quot;Spec/MRD Update Needed&quot;"/>
      <xsl:otherwise>
        <xsl:if test="&show-unmapped-custom-fields;">
          <xsl:element name="{name()}">
            <xsl:apply-templates select="@*"/>
            <xsl:apply-templates/>
          </xsl:element>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="customers-id">
    <xsl:param name="value"/>
    <xsl:element name="customers-id">
      <!-- <xsl:choose>
           ....   <xsl:when test="$value = &quot;&quot;">
           ....     <xsl:value-of select="../../reporter"/>
           ....   </xsl:when>
           ....   <xsl:when test="$value = &quot;N/A&quot;">
           ....     <xsl:value-of select="../../reporter"/>
           ....   </xsl:when>
           ....   <xsl:otherwise> -->
      <xsl:value-of select="$value"/>
      <!--   </xsl:otherwise>
           .... </xsl:choose> -->
    </xsl:element>
  </xsl:template>


  <xsl:template name="customer">
    <xsl:param name="value"/>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="$value = &quot;&quot;">
          <xsl:value-of select="../../reporter"/>
        </xsl:when>
        <xsl:when test="$value = &quot;N/A&quot;">
          <xsl:value-of select="../../reporter"/>
        </xsl:when>
        <xsl:when test="($value = &quot;EDIC&quot; or $value = &quot;CIGA&quot;)
                        and starts-with(../../key, 'FFS')">
          <xsl:value-of select="concat($value,' (FFS)')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$value"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="customer">
      <xsl:attribute name="full-name">
        <xsl:value-of select="$name"/>
      </xsl:attribute>
      <xsl:value-of select="$name"/>
    </xsl:element>
  </xsl:template>


  <xsl:template name="action">
    <xsl:param name="value"/>
    <xsl:element name="action">
      <xsl:choose>
        <xsl:when test="$value = &quot;&quot;">
          <xsl:value-of select="../../reporter"/>
        </xsl:when>
        <xsl:when test="$value = &quot;N/A&quot;">
          <xsl:value-of select="../../reporter"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$value"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>


  <xsl:template name="current-status">
    <xsl:param name="value"/>
    <xsl:element name="current-status">
      <xsl:value-of select="$value"/>
    </xsl:element>
  </xsl:template>


  <xsl:template name="urgency">
    <xsl:param name="value"/>
    <xsl:element name="urgency">
      <xsl:choose>
        <xsl:when test="$value = &quot;Immediate&quot;">
          <xsl:value-of select="'Imm'"/>
        </xsl:when>
        <xsl:when test="$value = &quot;High&quot;">
          <xsl:value-of select="'Hi'"/>
        </xsl:when>
        <xsl:when test="$value = &quot;Medium&quot;">
          <xsl:value-of select="'Med'"/>
        </xsl:when>
        <xsl:when test="$value = &quot;Low&quot;">
          <xsl:value-of select="'Low'"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$value"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>


  <!-- Print nodes unchanged -->
  <xsl:template match="*">
    <xsl:value-of select="$newline"/>
    <xsl:element name="{name()}">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <xsl:template match="@*">
    <xsl:attribute name="{name()}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>


  <xsl:template match="comment()">
    <xsl:value-of select="$newline"/>
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
    <xsl:value-of select="$newline"/>
  </xsl:template>

</xsl:transform>
