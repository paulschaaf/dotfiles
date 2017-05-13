#!/bin/sh

. `dirname $0`/set_cp.sh

java -classpath ${cp} com.guidewire.cc.integration.cmdline.ExportToolsMain -rules_plus exported_rules.xml $@