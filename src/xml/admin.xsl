<?xml version="1.0"?>
<!DOCTYPE config [
  <!ENTITY app-host                "localhost">
  <!ENTITY app-name                "cc">
  <!ENTITY app-port                "8080">

  <!ENTITY base-url                "http://&app-host;:&app-port;')">
  <!ENTITY url                     "&base-url;&app-name;">
  <!ENTITY login-url               "Login.do">
]
>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>

  <!-- ====================================================================
       ====================================================================
       Display Formatting
  -->
  <xsl:variable name="text-css">
    table {
       border-collapse: collapse;
       empty-cells:     show;
       font-size:       10px;
    }
    table.items {
       border-collapse: collapse;
       border: 2px solid black;
       empty-cells: show;
    }

    *.stats {
       border: 1px solid gray;
    }
    table.stats {
       border-collapse: collapse;
       cellpadding: 100;
       cellspacing: 100;
       table-layout: auto;
       width: auto;
    }

    tr.item-header {
       background-color: #9acd32;
       font-size: 125%;
    }
    tr:first-child.stats {
       background-color: lightgray;
    }

    a:link, a:visited, a:active {
       text-decoration: none;
    }
    a:hover {
       color: red;
       text-decoration: underline;
    }
  </xsl:variable>


  <!-- ==================================
       XSL search indexes
  -->
  <xsl:key name="role"          match="Role"          use="@public-id"/>
  <xsl:key name="roleprivilege" match="RolePrivilege" use="@public-id"/>
  <xsl:key name="group"         match="Group"         use="@public-id"/>
  <xsl:key name="security-zone" match="SecurityZone"  use="@public-id"/>
  <xsl:key name="user"          match="User"          use="@public-id"/>
  <xsl:key name="credential"    match="Credential"    use="@public-id"/>
  <xsl:key name="contact"       match="Person"        use="@public-id"/>
  <xsl:key name="company"       match="Company"       use="@public-id"/>
  <xsl:key name="address"       match="Address"       use="@public-id"/>

  
  <!-- ====================================================================
       ====================================================================
       XSL Templates
  -->
  <xsl:template match="/import">
    <html>
      <head>
        <title>
          <xsl:value-of select="'ClaimCenter Logins'"/>
        </title>
        <style type="text/css" media="*">
          <xsl:value-of select="$text-css"/>
        </style>
      </head>

      <body class="landscape">
        <table border="1" cellpadding="0" cellspacing="0" class="credentials">
          <xsl:apply-templates select="User">
            <xsl:sort select="key('contact',contact/@public-id)/lastname"/>
          </xsl:apply-templates>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="User">
    <tr>
      <td>
        <a>
          <xsl:apply-templates select="credential"/>
          <xsl:apply-templates select="contact"/>
        </a>
      </td>

      <td>
        <xsl:apply-templates select="roles/UserRole"/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="UserRole">
    <xsl:for-each select="key('role',role/@public-id)">
      <xsl:sort select="name" order="ascending"/>
      <a href="" title="{description}">
        <xsl:value-of select="name"/>
      </a>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="contact|credential|role">
    <xsl:apply-templates select="key(name(),@public-id)"/>
  </xsl:template>

  <xsl:template match="Person">
    <xsl:value-of select="concat(lastname,', ',firstname)"/>
  </xsl:template>

  <!-- <xsl:template match="Role">
  ....   <a href="." title="{description}">
  ....     <xsl:value-of select="name"/>
  ....   </a>
  .... </xsl:template> -->

  <xsl:template match="Credential">
    <xsl:variable name="real-password">
      <xsl:apply-templates select="password"/>
    </xsl:variable>
    <xsl:if test="string($real-password) != ''">
      <xsl:attribute name="href">
        <xsl:value-of select="$real-password"/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="password">
    <xsl:choose>
      <xsl:when test="password = 'nologin'"/>
      <xsl:when test="password = 'u6gO8+FQil44b/OHI9OxCnwdPTA='">
        <xsl:value-of select="concat('&login-url;?loginPassword=cc&amp;loginName=',username)"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>