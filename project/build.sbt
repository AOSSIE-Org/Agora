
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")


coverageEnabled := true
coverageMinimum := 70
coverageFailOnMinimum := true
coverageHighlighting := true
coverageOutputHTML := true
coverageOutputXML := true
coverageExcludedPackages := ".*Routes.*;.*Reverse.*;.*BuildInfo.*;.*Main.*"
