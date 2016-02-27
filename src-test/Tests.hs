import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)

import qualified ExtractDataTest as ExtractDataTest (allTests)
import qualified ExtractSchemaTest as ExtractSchemaTest (allTests)

tests = [
   ExtractDataTest.allTests,
   ExtractSchemaTest.allTests
   ]

main = defaultMain tests
