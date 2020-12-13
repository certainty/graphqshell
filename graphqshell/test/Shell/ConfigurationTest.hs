{-# LANGUAGE QuasiQuotes #-}

module Shell.ConfigurationTest where
import           Relude
import qualified Relude.Unsafe                 as Unsafe
import           Test.Tasty                     ( )
import           Test.Tasty.Hspec
import           Text.RawString.QQ
import           Text.URI                       ( render )
import           Shell.Configuration

spec_Configuration :: Spec
spec_Configuration = do
  describe "parseConfiguration" $ do
    it "returns valid configuration" $ do
      cfg <- parseConfiguration validConfiguration
      null (_appConfigEndpoints cfg) `shouldBe` False

    context "endpoints section" $ do
      it "parses the url correctly" $ do
        cfg <- parseConfiguration validConfiguration
        let endpoint = (Unsafe.head (_appConfigEndpoints cfg))
        render (_endpointURL endpoint) `shouldBe` "https://example.com/api"

      it "allows to omit custom headers" $ do
        cfg <- parseConfiguration validConfiguration
        let endpoint = (Unsafe.head $ Unsafe.tail (_appConfigEndpoints cfg))
        _endpointHttpConfig endpoint `shouldBe` Nothing

      it "fails if two endpoints are marked as default" $ do
        parseConfiguration twoDefaultEndpoints
          `shouldThrow` (== InvalidConfig
                          [MultipleDefaultEndpoints ["weather", "other"]]
                        )

      it "fails if two endpoints have the same name" $ do
        parseConfiguration duplicateEndpoints
          `shouldThrow` (== InvalidConfig [DuplicateNames ["weather"]])

    context "themes section" $ do
      it "fails if two themes are marked as default" $ do
        parseConfiguration twoDefaultThemes
          `shouldThrow` (== InvalidConfig
                          [MultipleDefaultThemes ["default", "other"]]
                        )

      it "fails if two themes have the same name" $ do
        parseConfiguration duplicateThemes
          `shouldThrow` (== InvalidConfig [DuplicateNames ["weather"]])

duplicateThemes :: ByteString
duplicateThemes = [r|
endpoints:
  - name: some
    default: true
    url: "https://example.com/api"
themes:
  - name: weather
    default: true
    path: /path/to/theme-file
  - name: weather
    path: /path/to/theme-file-foo
|]

duplicateEndpoints :: ByteString
duplicateEndpoints = [r|
endpoints:
  - name: weather
    default: true
    url: "https://example.com/api"
  - name: weather
    url: "https://example.com/api"
themes:
  - name: default
    default: true
    path: /path/to/theme-file
|]

twoDefaultThemes :: ByteString
twoDefaultThemes = [r|
endpoints:
  - name: weather
    default: true
    url: "https://example.com/api"
themes:
  - name: default
    default: true
    path: /path/to/theme-file
  - name: other
    default: true
    path: /path/to/theme-file
|]

twoDefaultEndpoints :: ByteString
twoDefaultEndpoints = [r|
endpoints:
  - name: weather
    default: true
    url: "https://example.com/api"
  - name: other
    default: true
    url: "http://example.com/api"
themes:
  - name: default
    default: true
    path: /path/to/theme-file
|]

validConfiguration :: ByteString
validConfiguration = [r|
endpoints:
  - name: weather
    default: true
    url: "https://example.com/api"
    link: "https://graphql-weather-api.herokuapp.com" 
    http:
      custom-headers:
        - name: "TEST_HEADER"
          value: "TEST_HEADER_VALUE"
  - name: other
    default: false
    url: "http://example.com/api"

themes:
  - name: default
    default: true
    path: /path/to/theme-file
|]
