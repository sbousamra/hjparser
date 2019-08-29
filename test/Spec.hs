import Test.Tasty
import Test.Tasty.HUnit
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [renderTests]

renderTests :: TestTree
renderTests = testGroup "Render Tests" [renderStringTest, renderObjectTest]

renderStringTest :: TestTree
renderStringTest = testCase "Render a string" $ do
  let subject = JString "Test string"
  let expected = "\"Test string\""
  let actual = render subject
  actual @?= expected
  
renderObjectTest :: TestTree
renderObjectTest = testCase "Render a object" $ do
  let subject = JObject [("test", JString "object"), ("key", JString "value")]
  let expected = "{ \"test\": \"object\",\n\"key\": \"value\" }"
  let actual = render subject
  actual @?= expected

parseStringTest :: TestTree
parseStringTest = testCase "Parse a string" $ do
  let subject = JString "Test string"
  let actual = parse $ render subject
  actual @?= Just subject