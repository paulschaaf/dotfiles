<?xml version="1.0"?>
<!DOCTYPE config [
  <!ENTITY jira-assignee                "pschaaf">

  <!--
  ....  Display
  -->
  <!ENTITY base-font-size               "11px">
  <!ENTITY small-font-percentage        "80">
  <!ENTITY large-font-percentage        "110">
  <!ENTITY font-family                  "Arial,Sans-Serif">
  <!ENTITY comment-display-length       "500">
  <!ENTITY highlight-idle               "true()">
  <!ENTITY highlight-with-markup        "true()">
  <!ENTITY show-assignee                "false()">
  <!ENTITY show-cust-id                 "true()">
  <!ENTITY show-flags                   "true()">
  <!ENTITY show-borders-within-groups   "false()">
  <!ENTITY show-customer-every-row      "false()">
  <!ENTITY show-issue-type              "false()">
  <!ENTITY show-issues-for-others       "true()">
  <!ENTITY show-or-hide-query-editor    "show">

  <!-- WARNING: Only possible if issues per customer issues are sorted by priority! -->
  <!ENTITY show-priority-icon-for-group "true()">

  <!ENTITY show-priority-repetitions    "true()">
  <!ENTITY show-tasks                   "false()">
  <!ENTITY table-header-color           "#9acd32">
  <!ENTITY use-styles                   "true()">

  <!--
  ....  Configuration
  -->
  <!ENTITY max-items                    "120">

  <!ENTITY assignee-field               "assignee">
  <!ENTITY customer-field               "customfield_10060">
  <!ENTITY key-field                    "issuekey">
  <!ENTITY priority-field               "priority">
  <!ENTITY status-field                 "status">

  <!ENTITY ascending                    "&amp;sorter/order=ASC">
  <!ENTITY descending                   "&amp;sorter/order=DESC">
  <!ENTITY sort-by                      "&amp;sorter/field=">
  <!ENTITY sort-by-assignee             "&sort-by;&assignee-field;&ascending;">
  <!ENTITY sort-by-customer             "&sort-by;&customer-field;&ascending;">
  <!ENTITY sort-by-key                  "&sort-by;&key-field;&ascending;">
  <!ENTITY sort-by-priority             "&sort-by;&priority-field;&descending;">
  <!ENTITY sort-by-status               "&sort-by;&status-field;&ascending;">

  <!-- Jira sorts fields are specified in reverse significance -->
  <!ENTITY sorted-by-cust-prior-key     "&sort-by-key;&sort-by-priority;&sort-by-customer;">
  <!ENTITY sorted-by-status-prior-key   "&sort-by-key;&sort-by-priority;&sort-by-status;">
  <!ENTITY sorted                       "&sorted-by-status-prior-key;">

  <!ENTITY assigned-to                  "&amp;assignee=">
  <!ENTITY assignee-type                "&amp;assigneeSelect=">
  <!ENTITY assigned-to-me               "&assignee-type;issue_current_user">
  <!ENTITY assigned-to-group            "&assignee-type;specificgroup">
  <!ENTITY assigned-in-support          "&assigned-to-group;&assigned-to;support">
  <!ENTITY with-assignee                "&assignee-type;specificuser&assigned-to;">
  <!ENTITY with-customer                "&amp;&customer-field;=">

  <!ENTITY unresolved                   "&amp;resolution=-1">
  <!ENTITY with-jira-status             "&amp;statusIds=">
  <!ENTITY with-open-status             "&with-jira-status;1">
  <!ENTITY with-in-progress-status      "&with-jira-status;3">
  <!ENTITY with-reopened-status         "&with-jira-status;4">
  <!ENTITY with-resolved-status         "&with-jira-status;5">
  <!ENTITY with-closed-status           "&with-jira-status;6">

  <!ENTITY updated-in-last-7-days       "&amp;updatedPrevious=604800000">
  <!ENTITY updated-in-last-30-days      "&amp;updatedPrevious=2592000000">

  <!ENTITY in-project                   "&amp;pid=">
  <!ENTITY in-CC                        "&in-project;10000">
  <!ENTITY in-FFS                       "&in-project;10040">
  <!ENTITY in-PG                        "&in-project;10041">
  <!ENTITY in-SS                        "&in-project;10080">

  <!ENTITY as-rss                       "&amp;decorator=none&amp;view=rss">
  <!ENTITY jira-home                    "http://jira/jira">
  <!ENTITY jira-list
           "&jira-home;/secure/IssueNavigator.jspa?reset=true&amp;mode=&show-or-hide-query-editor;&amp;tempMax=&max-items;&sorted;">
  <!ENTITY jira-unclosed                "&jira-list;&with-open-status;&with-in-progress-status;&with-reopened-status;&with-resolved-status;">
  <!ENTITY jira-unresolved              "&jira-list;&unresolved;">
  <!ENTITY jira-with-number             "&jira-home;/browse">
  <!ENTITY jira-closed-or-resolved      "&jira-list;&with-closed-status;&with-resolved-status;">

]
>

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>

  <xsl:variable name="show-only">
    <!-- <xsl:value-of select="'idle'"/> -->
    <!-- <xsl:value-of select="'old'"/> -->
    <!-- <xsl:value-of select="'Canal'"/> -->
    <!-- <xsl:value-of select="'CIGA'"/> -->
    <!-- <xsl:value-of select="'CNA'"/> -->
    <!-- <xsl:value-of select="'EDIC'"/> -->
    <!-- <xsl:value-of select="'ICAT'"/> -->
    <!-- <xsl:value-of select="'Liberty Surety'"/> -->
    <!-- <xsl:value-of select="'MSA'"/> -->
    <!-- <xsl:value-of select="'NJM'"/> -->
  </xsl:variable>


  <!-- ==================================
       XSL search indexes
  -->
  <xsl:key name="actions"    match="item" use="action"/>
  <xsl:key name="customers"  match="item" use="customer"/>
  <xsl:key name="priorities" match="item" use="priority"/>


  <!-- ====================================================================
       ====================================================================
       XSL Templates
  -->

  <xsl:template match="/feed">
    <xsl:call-template name="html-document"/>
  </xsl:template>


  <xsl:template name="html-document">
    <html>
      <head>
        <title>
          <xsl:value-of select="concat('Jiras ',description)"/>
        </title>
        <link rel="shortcut icon"
              href="jira.png"/>
        <link rel="icon"
              type="image/png"
              href="jira.png"/>
        <xsl:if test="&use-styles;">
          <style type="text/css"
                 media="*">
            BODY.landscape {
               writing-mode:      tb-rl;
               <!-- height:       &small-font-percentage;%; -->
               margin:            0% 0%;
            }
            BODY.landscape table {
            <!-- margin-right: 80pt; -->
               filter:            progid:DXImageTransform.Microsoft.BasicImage(Rotation=1);
            }

            BODY {
               font-family:       &font-family;;
               leftmargin:        0;
               topmargin:         0;
               marginwidth:       0;
               marginheight:      0;
            }

            table {
               border-collapse:   collapse;
               empty-cells:       show;
               font-size:         &base-font-size;;
            }

            table.items {
               <!-- border-collapse:   collapse; -->
               border:            2px solid black;
               <!-- empty-cells:       show; -->
            }

            tr.item-header {
               background-color:  &table-header-color;;
               font-size:         125%;
            }

            <!-- td.new-cust {
            ....    border-left:       2px solid black;
            .... } -->
            td.same-cust {
               <xsl:if test="not(&show-borders-within-groups;)">
                 border-top:        hidden;
               </xsl:if>
            }
            tr.new-cust {
               border-top:        2px solid black;
               text-align:        center;
               vertical-align:    top;
            }
            a.new-cust {
               font-size:         120%;
               font-weight:       bold;
            }

            *.stats {
               border:            1px solid gray;
            }
            table.stats {
               <!-- border-collapse:   collapse; -->
               cellpadding:       100;
               cellspacing:       100;
               table-layout:      auto;
               width:             auto;
            }
            tr.stats-header {
               background-color:  lightgray;
               <xsl:if test="not(&show-borders-within-groups;)">
                 border-top:        hidden;
               </xsl:if>
            }
            tr:first-child.stats {
               background-color:  lightgray;
            }

            *.total {
               align:             right;
               font-weight:       bold;
               <xsl:if test="not(&show-borders-within-groups;)">
                 border-right:      hidden;
                 border-bottom:     hidden;
               </xsl:if>
            }
            th.total {
               border-left:       1px solid gray;
               <xsl:if test="not(&show-borders-within-groups;)">
                 border-top:        hidden;
               </xsl:if>
               color:             white;
               background-color:  white;
            }
            tr.total {
               border-top:        2px solid gray;
               <xsl:if test="not(&show-borders-within-groups;)">
                 border-left:       hidden;
               </xsl:if>
            }
            td.total {
               text-align:        right;
               border-top:        2px solid gray;
            }

            td.per-customer-link {
               font-size:         100%;
               text-align:        left;
               text-indent:       1em;
            }

            <!-- a:link,visited,active,hover -->
            a.key, a.new-cust {
               text-decoration:   none;
            }
            a.Closed {
               font-size:         &small-font-percentage;%;
               text-decoration:   line-through;
            }
            <!-- a:hover.nolink { } -->

            <xsl:if test="&highlight-with-markup;">

              td.Alert {
                 color:           red;
                 font-size:       120%;
                 font-weight:     bold;
              }
              td.AssignedToOther {
                 color:           gray;
                 font-size:       &small-font-percentage;%;
              }
              td.Closed {
                 color:           lightgray;
                 text-decoration: line-through;
              }
              td.Confirm-close {
                 color:           gray;
                 text-decoration: line-through;
              }
              td.Customer {
                 color:           gray;
                 font-size:       100%;
              }
              td.Flag {
                 font-size:       125%;
                 font-weight:     bold;
              }
              td.idle {
               <xsl:if test="&highlight-idle;">
                 color:           red;
               </xsl:if>
              }
              td.ProdSvcs {
                 color:           gray;
                 font-size:       &small-font-percentage;%;
              }
              td.Placeholder {
                 color:           red;
                 font-size:       &large-font-percentage;%;
              }
              td.Support {
                 font-weight:     bold;
              }
              td.Wait {
                 color:           gray;
              }
              td.Workaround {
                 color:           lightgray;
                 text-decoration: line-through;
              }

            </xsl:if>
          </style>
        </xsl:if>
        <style type="text/css" media="print">
          BODY {
          <!-- td.Placeholder {
               ....    color: lightgray;
               ....    font-size: 5%;
               ....    text-decoration: line-through;
               .... } -->
          <!-- writing-mode: tb-rl;
               .... height: &small-font-percentage;%; -->
          <!-- margin: 0% 0%; -->
          }
          BODY.landscape table {
          <!-- margin-right: 80pt;
               .... filter: progid:DXImageTransform.Microsoft.BasicImage(Rotation=1); -->
          }
        </style>
      </head>

      <body class="landscape">
        <xsl:call-template name="report-header"/>
        <xsl:if test="&show-tasks;">
          <xsl:call-template name="table-of-tasks"/>
          <br/>
        </xsl:if>
        <xsl:call-template name="table-of-items"/>
      </body>
    </html>
  </xsl:template>


  <xsl:template name="get-style">
    <xsl:choose>
      <xsl:when test="starts-with(support-action,'Close')">
        Closed
      </xsl:when>
      <xsl:when test="support-action = 'Confirm close'">
        Confirm-close
      </xsl:when>
      <xsl:when test="support-action = 'Placeholder'">
        Placeholder
      </xsl:when>
      <xsl:when test="support-action = 'Workaround'">
        Workaround
      </xsl:when>
      <xsl:when test="starts-with(support-action,'!')">
        Flag
      </xsl:when>
      <xsl:when test="starts-with(support-action,'Wait')">
        Wait
      </xsl:when>
      <xsl:when test="assignee[@username != '&jira-assignee;']">
        AssignedToOther
      </xsl:when>
      <xsl:when test="not(support-action) or string(support-action) = ''">
        Alert
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="action"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="table-of-items">
    <xsl:variable name="title">
      <xsl:choose>
        <xsl:when test="$show-only = 'idle'">
          Idle Issues
        </xsl:when>
        <xsl:when test="$show-only = 'old'">
          Old Issues
        </xsl:when>
        <xsl:when test="string($show-only) = ''">
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('Issues for ', $show-only)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="string($title) != ''">
      <h3><xsl:value-of select="$title"/></h3>
    </xsl:if>
    <table border="1" cellpadding="0" cellspacing="0" class="items">
      <col span="1" class="items"/>
      <tr height="0" class="item-header">
        <th>Cust</th>
        <th>Pr</th>
        <th>Ur</th>
        <xsl:if test="&show-issue-type;">
          <th>Type</th>
        </xsl:if>
        <th>Item</th>
        <xsl:if test="string($show-only) = '' and &show-flags;">
          <th>Flags</th>
        </xsl:if>
        <th>Updated</th>
        <th>Created</th>
        <xsl:if test="&show-assignee;">
          <th>Assignee</th>
        </xsl:if>
        <th width="25%">Title</th>
        <th>Action Owner</th>
        <th>Support Action</th>
        <th width="100%">Next Action</th>
      </tr>
      <xsl:call-template name="items-grouped-by-customer"/>
    </table>
  </xsl:template>


  <xsl:template name="table-of-tasks">
    <table border="1" cellpadding="0" cellspacing="0" class="items">
      <tr height="0" class="item-header">
        <th>Cust</th>
        <th>Pr</th>
        <th>Ur</th>
        <th>Item</th>
        <th>Updated</th>
        <th>Created</th>
        <th width="25%">Title</th>
        <th>Action Owner</th>
        <th>Support Action</th>
        <th width="100%">Next Action</th>
      </tr>
      <xsl:for-each select="task">
        <tr>
          <xsl:apply-templates select="customer"/>
          <xsl:apply-templates select="priority"/>
          <xsl:apply-templates select="."/>
        </tr>
      </xsl:for-each>
    </table>
  </xsl:template>


  <!-- <xsl:template match="task">
  ....   <tr>
  ....     <td>
  ....       <xsl:value-of select="customer"/>
  ....     </td>
  ....     <td><xsl:apply-templates select="priority"/></td>
  ....     <td><xsl:apply-templates select="updated"/></td>
  ....     <td><xsl:apply-templates select="created"/></td>
  ....     <td><xsl:apply-templates select="summary"/></td>
  ....     <td><xsl:apply-templates select="assignee"/></td>
  ....     <td><xsl:apply-templates select="action"/></td>
  ....     <td><xsl:apply-templates select="support-action"/></td>
  ....     <td><xsl:apply-templates select="action-detail"/></td>
  ....   </tr>
  .... </xsl:template> -->


  <xsl:template name="items-grouped-by-customer">
    <!-- Muench method for grouping
         To iterate through the customers:
         for each item
         use key() to retrieve all items with the same customer
         generate an id for the first node and the current node
         if they match, then the nodes are the same
    -->
    <xsl:for-each select="item[generate-id(.)=
                          generate-id(key('customers', customer)[1])]">
      <xsl:sort select="customer"/>
      <!-- <xsl:sort select="priority"/> -->
      <!-- <xsl:sort select="key"/> -->

        <xsl:for-each select="key('customers', customer)">
          <!-- <xsl:sort select="action" order="descending"/> -->
          <xsl:sort select="priority"/>
          <!-- <xsl:sort select="customers-id"/> -->
          <!-- <xsl:sort select="key"/> -->

          <xsl:if test="true() or assignee[@username = '&jira-assignee;'] or &show-issues-for-others;">
            <xsl:variable name="prior" select="priority"/>

            <xsl:if test="starts-with(customer, $show-only)
                          or ($show-only = 'idle')
                          or ($show-only = 'old')">
              <tr align="center" valign="top" height="0">

                <!-- TODO: THIS IS A VISCIOUS HACK! The call to preceding-sibling::
                     requires us to assume the target document is already sorted by customer -->
                <xsl:choose>
                  <xsl:when test="&show-customer-every-row;
                                  or (position() = 1)
                              or (customer != preceding-sibling::item[1]/customer)">
                    <xsl:apply-templates select="customer"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <td class="same-cust"/>
                  </xsl:otherwise>
                </xsl:choose>

                <!-- TODO: THIS IS A VISCIOUS HACK! The call to preceding-sibling::
                     requires us to assume the target document is already sorted, either
                     by priority or by customer and then priority-->
                <xsl:if test="not(&show-priority-icon-for-group;)
                              or (position() = 1)
                              or (priority != preceding-sibling::item[1]/priority)">
                  <xsl:variable name="repetitions">
                    <xsl:value-of select="count(key('customers', customer)[priority = $prior])"/>
                  </xsl:variable>
                  <td valign="top">
                    <xsl:if test="&show-priority-icon-for-group;">
                      <xsl:attribute name="rowspan">
                        <xsl:value-of select="$repetitions"/>
                      </xsl:attribute>
                    </xsl:if>
                    <xsl:apply-templates select="priority"/>
                    <xsl:if test="&show-priority-repetitions;">
                      <xsl:value-of select="concat('*',$repetitions)"/>
                    </xsl:if>
                  </td>
                </xsl:if>

                <xsl:if test="assignee[@username = '&jira-assignee;'] or &show-issues-for-others;">
                  <xsl:if test="not($show-only)
                                or (($show-only = 'idle') and updated[@idle])
                                or (($show-only = 'old')  and created[@old])
                                or (starts-with(customer, $show-only))">
                    <xsl:apply-templates select="."/>
                  </xsl:if>
                </xsl:if>
              </tr>
            </xsl:if>
          </xsl:if>
        </xsl:for-each>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="item|task">
    <xsl:variable name="style">
      <xsl:call-template name="get-style"/>
    </xsl:variable>

    <td class="{$style}">
      <xsl:apply-templates select="urgency"/>
    </td>
    <xsl:if test="&show-issue-type;">
      <td class="{$style}"   nowrap="true">
        <xsl:apply-templates select="type"/>
      </td>
    </xsl:if>
    <td class="{$style}"     nowrap="true">
      <xsl:apply-templates   select="key"/>
      <xsl:if test="&show-cust-id; and customers-id">
        <br/>(<xsl:apply-templates select="customers-id"/>)
      </xsl:if>
    </td>
    <xsl:if test="string($show-only) = '' and &show-flags;">
      <td>
        <xsl:call-template name="flags"/>
      </td>
    </xsl:if>
    <td class="{$style}">
      <xsl:apply-templates select="updated"/>
    </td>
    <td class="{$style}">
      <xsl:apply-templates select="created"/>
    </td>
    <xsl:if test="&show-assignee;">
      <td class="{$style}"   nowrap="true">
        <xsl:apply-templates select="assignee"/>
      </td>
    </xsl:if>
    <td align="left" class="{$style}">
      <span title="{description}"><xsl:apply-templates select="summary"/></span>
    </td>
    <td class="{$style}">
      <xsl:apply-templates select="action"/>
    </td>
    <td class="{$style}">
      <xsl:call-template name="support-action"/>
    </td>
    <td class="{$style}">
      <xsl:call-template name="action-detail"/>
    </td>
  </xsl:template>


  <xsl:template match="customer">
    <xsl:attribute name="class">new-cust</xsl:attribute>
    <xsl:variable name="updated-in-previous-url"/>
    <td class="new-cust">
      <table border="0">
        <tr>
          <td nowrap="true">
            <xsl:call-template name="link-for-customer">
              <xsl:with-param name="customer" select="."/>
            </xsl:call-template>
            <xsl:if test="$show-only">
              <xsl:value-of select="concat(': ',count(key('customers', .)))"/>
            </xsl:if>
          </td>
        </tr>
        <xsl:if test="string($show-only) = ''">
          <tr>
            <td class="per-customer-link">
              <a class="Closed" target="_blank" title="{.} Resolved or Closed in Last 7 Days" href="&jira-closed-or-resolved;&updated-in-last-7-days;&with-customer;{@full-name}">Week</a>
            </td>
          </tr>
          <tr>
            <td class="per-customer-link">
              <a class="Closed" target="_blank" title="{.} Resolved or Closed in Last 30 Days" href="&jira-closed-or-resolved;&updated-in-last-30-days;&with-customer;{@full-name}">Month</a>
            </td>
          </tr>
        </xsl:if>
      </table>
    </td>
  </xsl:template>


  <xsl:template name="link-for-customer">
    <xsl:param name="customer"/>
    <a target="_blank"
       name="{$customer}"
       title="View {$customer} issues in Jira"
       class="new-cust"
       href="&jira-unclosed;&in-FFS;&in-PG;&in-SS;&with-customer;{@full-name}">
      <xsl:value-of select="."/>
    </a>
  </xsl:template>


  <xsl:template match="key">
    <a class="key"
       target="_blank"
       href="&jira-with-number;/{.}">
      <xsl:value-of select="."/>
    </a>
  </xsl:template>


  <xsl:template match="priority">
    <xsl:element name="img">
      <xsl:attribute name="src">
        <xsl:value-of select="'&jira-home;/images/icons/priority_'"/>
        <xsl:choose>
          <xsl:when test=". = 'Blocker'">
            <xsl:value-of select="'blocker'"/>
          </xsl:when>
          <xsl:when test=". = 'Critical'">
            <xsl:value-of select="'critical'"/>
          </xsl:when>
          <xsl:when test=". = 'Major'">
            <xsl:value-of select="'major'"/>
          </xsl:when>
          <xsl:when test=". = 'Minor'">
            <xsl:value-of select="'minor'"/>
          </xsl:when>
          <xsl:when test=". = 'Trivial'">
            <xsl:value-of select="'trivial'"/>
          </xsl:when>
        </xsl:choose>
        <xsl:value-of select="'.gif'"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- <xsl:template match="urgency">
  ....   <xsl:choose>
  ....     <xsl:when test=". = 999"/>
  ....     <xsl:otherwise>
  ....       <xsl:value-of select="."/>
  ....     </xsl:otherwise>
  ....   </xsl:choose>
  .... </xsl:template> -->


  <xsl:template match="customers-id|updated">
    <xsl:value-of select="."/>
  </xsl:template>


  <xsl:template match="created">
    <xsl:value-of select="."/>
    <br/>
    <xsl:apply-templates select="../reporter"/>
  </xsl:template>


  <xsl:template match="reporter">
    <xsl:value-of select="@username"/>
  </xsl:template>


  <xsl:template name="flags">
    <xsl:if test="updated[@idle]">
      <xsl:attribute name="class">idle</xsl:attribute>
      <xsl:text>Idle </xsl:text>
      <xsl:if test="created[@old]">
        <br/>
      </xsl:if>
    </xsl:if>
    <xsl:if test="created[@old]">
      <xsl:text>Old</xsl:text>
    </xsl:if>
  </xsl:template>


  <xsl:template match="action">
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="../assignee[@username != '&jira-assignee;']">
        <xsl:value-of select="concat(' [',../assignee,']')"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="action-detail">
    <xsl:attribute name="align">left</xsl:attribute>
    <xsl:attribute name="valign">top</xsl:attribute>
    <xsl:if test="string(action-detail) != '' and string(action-detail) != '.'">
      <xsl:apply-templates select="action-detail"/>
      <xsl:if test="comment">
        <br/>
      </xsl:if>
    </xsl:if>
    <xsl:if test="current-status">
      <span title="{@created}" style="font-weight: normal">
        <b>Status: </b>
        <xsl:apply-templates select="current-status"/>
      </span>
      <br/>
    </xsl:if>
    <xsl:apply-templates select="comment"/>
  </xsl:template>


  <xsl:template name="support-action">
    <xsl:attribute name="valign">top</xsl:attribute>
    <xsl:apply-templates select="support-action"/>
    <xsl:if test="not(support-action) and ../assignee[@username != 'pschaaf']">
      <xsl:value-of select="'N/A'"/>
    </xsl:if>
  </xsl:template>


  <xsl:template match="comment">
    <!-- {substring($cmt,&comment-display-length;+1,1)}"> -->
    <!-- substring($cmt,1,&comment-display-length;)"/> -->
    <xsl:if test="@level = ''">
      <span title="{@created}" style="font-weight: normal">
        <u>
          <xsl:value-of select="@created"/>
          <xsl:if test="@author != '&jira-assignee;'">
            <xsl:value-of select="concat(' ', @author)"/>
          </xsl:if>
        </u>
        <i>
          <xsl:value-of select="concat(': ', substring(.,1,&comment-display-length;+1))"/>
        </i>
      </span>
    </xsl:if>
  </xsl:template>


  <xsl:template match="*">
    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template name="report-header">
    <xsl:call-template name="stats-tables"/>
    <xsl:if test="string($show-only) = ''">
      <xsl:call-template name="list-of-links"/>
    </xsl:if>
  </xsl:template>


  <xsl:template name="stats-tables">
    <table border="0" cellpadding="5" cellspacing="0">
      <tr valign="top">
        <td>
          <xsl:call-template name="table-of-actions"/>
        </td>
        <td>
          <xsl:call-template name="table-of-priorities"/>
        </td>
        <td>
          <xsl:call-template name="table-of-flags"/>
        </td>
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="table-of-actions">
    <table class="stats">
      <tr class="stats">
        <th class="stats" rowspan="2">Customer</th>

        <xsl:for-each select="item[generate-id(.)=
                              generate-id(key('actions', action)[1])]">
          <xsl:sort select="action"/>
          <xsl:variable name="act" select="action"/>
          <th align="center" class="stats" rowspan="2">
            <xsl:value-of select="action"/>
          </th>
        </xsl:for-each>

        <th align="center" class="total" rowspan="2">
          <br/>
          total
        </th>
      </tr>
      <tr/>

      <xsl:for-each select="item[generate-id(.)=
                            generate-id(key('customers', customer)[1])]">
        <xsl:sort select="customer"/>

        <xsl:variable name="cust" select="customer"/>
        <tr class="stats">
          <td nowrap="true">
            <a href="#{customer}"
               title="Jump to {customer} issues">
              <xsl:value-of select="customer"/>
            </a>
          </td>

          <xsl:for-each select="//item[generate-id(.)=
                                generate-id(key('actions', action)[1])]">
            <xsl:sort select="action"/>
            <xsl:variable name="act" select="action"/>
            <td align="right" class="stats">
              <xsl:variable name="num">
                <xsl:value-of select="count(../item[customer = $cust and action = $act])"/>
              </xsl:variable>
              <xsl:if test="$num > 0">
                <xsl:value-of select="$num"/>
              </xsl:if>
            </td>
          </xsl:for-each>

          <td class="total">
            <!-- <xsl:value-of select="count(../item[customer = $cust])"/> -->
          </td>
        </tr>
      </xsl:for-each>

      <tr class="total">
        <td class="total">total:</td>
        <xsl:for-each select="item[generate-id(.)=
                              generate-id(key('actions', action)[1])]">
          <xsl:sort select="action"/>
          <xsl:variable name="act" select="action"/>
          <td class="total">
            <xsl:value-of select="count(../item[action = $act])"/>
          </td>
        </xsl:for-each>

        <!-- <td class="total">
        ....   <xsl:value-of select="count(item)"/>
        .... </td> -->
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="table-of-priorities">
    <table class="stats">
      <col id="names" class="stats"/>
      <col class="stats">
        <xsl:attribute name="span">
          <xsl:value-of select="count(item[generate-id(.)=
                                generate-id(key('priorities', priority))])"/>
        </xsl:attribute>
      </col>

      <tr class="stats">

        <xsl:for-each select="item[generate-id(.)=
                              generate-id(key('priorities', priority)[1])]">
          <xsl:sort select="priority"/>
          <th align="center" class="stats" rowspan="2">
            <xsl:value-of select="priority"/>
          </th>
        </xsl:for-each>

        <th align="center" class="total" rowspan="2"><br/>
        total
        </th>
      </tr>
      <tr/>

      <xsl:for-each select="item[generate-id(.)=
                            generate-id(key('customers', customer)[1])]">
        <xsl:sort select="customer"/>
        <xsl:variable name="item" select="item"/>
        <xsl:variable name="cust" select="customer"/>
        <tr class="stats">

          <xsl:for-each select="//item[generate-id(.)=
                                generate-id(key('priorities', priority)[1])]">
            <xsl:sort select="priority"/>
            <xsl:variable name="prio" select="priority"/>
            <td align="right">
              <xsl:variable name="num">
                <xsl:value-of select="count(../item[customer = $cust and priority = $prio])"/>
              </xsl:variable>
              <xsl:if test="$num > 0">
                <xsl:value-of select="$num"/>
              </xsl:if>
            </td>
          </xsl:for-each>

          <td align="right" class="total">
            <!-- <xsl:value-of select="count(../item[customer = $cust])"/> -->
          </td>
        </tr>
      </xsl:for-each>

      <tr class="total">
        <xsl:for-each select="item[generate-id(.)=
                              generate-id(key('priorities', priority)[1])]">
          <xsl:sort select="priority"/>
          <xsl:variable name="prio" select="priority"/>
          <td class="total">
            <xsl:value-of select="count(../item[priority = $prio])"/>
          </td>
        </xsl:for-each>
        <!-- <td class="total">
             ....   <xsl:value-of select="count(item)"/>
             .... </td> -->
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="table-of-flags">
    <table class="stats">
      <col span="5" class="stats"/>

      <tr class="stats">
        <th align="center" colspan="2">Active</th>
        <th align="center" colspan="2">Idle</th>
        <th align="center" class="total" rowspan="2">total</th>
      </tr>
      <tr class="stats-header">
        <th align="center">New</th>
        <th align="center">Old</th>
        <th align="center">New</th>
        <th align="center">Old</th>
      </tr>

      <xsl:for-each select="item[generate-id(.)=
                            generate-id(key('customers', customer)[1])]">
        <xsl:sort select="customer"/>
        <xsl:variable name="item" select="item"/>
        <xsl:variable name="cust" select="customer"/>
        <tr class="stats">

          <td align="right">
            <xsl:variable name="nn">
              <xsl:value-of select="count(../item[customer = $cust
                                    and (not(updated[@idle]) and not(created[@old]))])"/>
            </xsl:variable>
            <xsl:if test="$nn > 0">
              <xsl:value-of select="$nn"/>
            </xsl:if>
          </td>

          <td align="right">
            <xsl:variable name="ny">
              <xsl:value-of select="count(../item[customer = $cust
                                    and (not(updated[@idle]) and created[@old])])"/>
            </xsl:variable>
            <xsl:if test="$ny > 0">
              <xsl:value-of select="$ny"/>
            </xsl:if>
          </td>

          <td align="right">
            <span style="color:red">
              <xsl:variable name="yn">
                <xsl:value-of select="count(../item[customer = $cust
                                      and (updated[@idle] and not(created[@old]))])"/>
              </xsl:variable>
              <xsl:if test="$yn > 0">
                <xsl:value-of select="$yn"/>
              </xsl:if>
            </span>
          </td>

          <td align="right">
            <span style="color:red">
              <xsl:variable name="yy">
                <xsl:value-of select="count(../item[customer = $cust
                                      and (updated[@idle] and created[@old])])"/>
              </xsl:variable>
              <xsl:if test="$yy > 0">
                <xsl:value-of select="$yy"/>
              </xsl:if>
            </span>
          </td>

          <td align="right" class="total">
            <xsl:value-of select="count(../item[customer = $cust])"/>
          </td>
        </tr>
      </xsl:for-each>
      <tr class="total">
        <td class="total">
          <span style="">
            <xsl:value-of select="count(item[not(updated[@idle]) and not(created[@old])])"/>
          </span>
        </td>
        <td class="total">
          <span style="">
            <xsl:value-of select="count(item[not(updated[@idle]) and     created[@old]])"/>
          </span>
        </td>
        <td class="total">
          <span style="color:red">
            <xsl:value-of select="count(item[    updated[@idle]  and not(created[@old])])"/>
          </span>
        </td>
        <td class="total">
          <span style="color:red">
            <xsl:value-of select="count(item[    updated[@idle]  and     created[@old]])"/>
          </span>
        </td>

        <td class="total">
          <xsl:value-of select="count(item)"/>
        </td>
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="list-of-links">
    <table border="0" cellpadding="10">
      <tr>
        <td nowrap="true">
          <a href="&jira-home;"
             target="_blank">
            <xsl:text>Jira</xsl:text>
          </a>
          <xsl:text> </xsl:text>
          <a target="_blank">
            <xsl:attribute name="href">
              <xsl:value-of select="url"/>
            </xsl:attribute>
            snapshot
          </a>
          <xsl:text> taken </xsl:text>
          <u><xsl:value-of select="/feed/updated"/></u>
        </td>
        <td nowrap="true">
          <a href="&jira-home;/secure/CreateIssue!default.jspa"
             target="_blank">
            <xsl:text>Create Issue</xsl:text>
          </a>
        </td>
        <td>
          Closed/Resolved in last:
          <a target="_blank"
             href="&jira-closed-or-resolved;&updated-in-last-7-days;&assigned-to-me;">
            <xsl:text>Week</xsl:text>
          </a>,
          <a target="_blank"
             href="&jira-closed-or-resolved;&updated-in-last-30-days;&assigned-to-me;">
            <xsl:text>Month</xsl:text>
          </a>
        </td>
        <td>
          <a target="_blank"
             href="http://wiki/index.php/CustomerConfigurations#ClaimCenter_Customer">
            <xsl:text>Customer Configurations</xsl:text>
          </a>
        </td>
      </tr>
    </table>
  </xsl:template>


  <xsl:template match="@*">
    <xsl:attribute name="{name()}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

</xsl:stylesheet>
