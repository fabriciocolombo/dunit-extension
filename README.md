Features
=
Extended check methods with specific types and custom message format:

    -  CheckEqualsDate
    -  CheckEqualsDouble
    -  CheckGreaterThanExpected
    -  CheckGreaterThanOrEqualsExpected
    -  CheckIsEmptyString
    -  CheckNotIsEmptyString
    -  CheckEqualsEnum
    -  CheckEqualsText
 
Support to filter test cases to run using application parameters:
  
    Usage:  -TestCases ClassA;ClassB;ClassC 

Compatibility instructions
------------------------------------

Dunit-extension requires that the DUnit framework be downloaded from svn trunk
(http://dunit.svn.sourceforge.net/svnroot/dunit), because the latest file avaliable 
for download(dunit-9.3.0.zip) does not contain a unit(CIXMLTestRunner) that generates the Xml Report for Continous 
integration tools.
The "dunit\private\plandolt" directory should be included in the Delphi library search path.

