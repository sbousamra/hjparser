import Test.Tasty
import Test.Tasty.HUnit
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [renderTests]

renderTests :: TestTree
renderTests = testGroup "Render Tests" 
  [ renderStringTest
  , renderNumberTest
  , renderBoolTest
  , renderNullTest
  , renderObjectTest
  , renderArrayTest
  ]

renderStringTest :: TestTree
renderStringTest = testCase "Render a string" $ do
  let subject = JString "Test string"
  let expected = "\"Test string\""
  let actual = render subject
  actual @?= expected

renderNumberTest :: TestTree
renderNumberTest = testCase "Render a number" $ do
  let subject = JNumber 1.0
  let expected = "1.0"
  let actual = render subject
  actual @?= expected
  
renderBoolTest :: TestTree
renderBoolTest = testCase "Render a bool" $ do
  let subject = JBool True
  let expected = "true"
  let actual = render subject
  actual @?= expected

renderNullTest :: TestTree
renderNullTest = testCase "Render a null" $ do
  let subject = JNull
  let expected = "null"
  let actual = render subject
  actual @?= expected
  
renderObjectTest :: TestTree
renderObjectTest = testCase "Render a object" $ do
  let subject = JObject [("test", JString "object"), ("key", JString "value")]
  let expected = "{ \"test\": \"object\",\n\"key\": \"value\" }"
  let actual = render subject
  actual @?= expected

renderArrayTest :: TestTree
renderArrayTest = testCase "Render a array" $ do
  let subject = 
        JArray [
          JObject [("test", JString "array"), ("key", JString "value")]
        ]
  let expected = "[{ \"test\": \"array\",\n\"key\": \"value\" }]"
  let actual = render subject
  actual @?= expected

parseStringTest :: TestTree
parseStringTest = testCase "Parse a string" $ do
  let subject = JString "Test string"
  let actual = parse $ render subject
  actual @?= Just subject