Features
======

Asserts
-----------
Extended check methods with specific types and custom message format:

    -  CheckEqualsDate
    -  CheckEqualsDouble
    -  CheckGreaterThanExpected
    -  CheckGreaterThanOrEqualsExpected
    -  CheckIsEmptyString
    -  CheckNotIsEmptyString
    -  CheckEqualsEnum
    -  CheckEqualsText

Category
--------------- 
Support to filter test cases to run using application parameters:
  
    Usage:  -TestCases ClassA;ClassB;ClassC 

Register Suit
------------------
Helper class methods to easy register TestCases

    TTestClass.RegisterTest();
    TTestClass.RegisterRepeatedTest(100');

Runner
----------

You can use TTestRunnerUtils to easly run your text. TTestRunnerUtils automaticaly detect command line arguments in order to perform the tests using different test runners.
This it is the currently accepted command line arguments:

 * No args - GUI Test Runner [Default]
 * -text or /text - Run using TextTestRunner
 * -xml or /xml - Run using CIXMLTestRunner. When using xml mode, there are an optional parameter `-output test_ouput_directory`. The default is use the directory `..\target\surefire-reports`.