{-# LANGUAGE QuasiQuotes #-}

module Shell.ConfigurationTest where
import           Relude
import qualified Relude.Unsafe                 as Unsafe
import           Test.Tasty                     ( )
import           Test.Tasty.Hspec
import           Text.RawString.QQ
import           Text.URI                       ( render )
import           Shell.Configuration
import           Lens.Micro.Platform            ( (^.) )

spec_Configuration :: Spec
spec_Configuration = do
  describe "parseConfiguration" $ do
    it "returns valid configuration" $ do
      cfg <- parseConfiguration "/test" validConfiguration
      null (cfg ^. appConfigEndpoints) `shouldBe` False

    context "application section" $ do
      it "extract tickRate" $ do
        cfg <- parseConfiguration "/test" validConfiguration
        (cfg ^. appConfigTickRate) `shouldBe` (Just 10000)

    context "endpoints section" $ do
      it "parses the url correctly" $ do
        cfg <- parseConfiguration "/test" validConfiguration
        let endpoint = (Unsafe.head (_appConfigEndpoints cfg))
        render (endpoint ^. endpointURL) `shouldBe` "https://example.com/api"

      it "allows to omit custom headers" $ do
        cfg <- parseConfiguration "/test" validConfiguration
        let endpoint = (Unsafe.head $ Unsafe.tail (cfg ^. appConfigEndpoints))
        (endpoint ^. endpointHttpConfig) `shouldBe` Nothing

      it "fails if two endpoints are marked as default" $ do
        parseConfiguration "/test" twoDefaultEndpoints
          `shouldThrow` (== InvalidConfig
                          [MultipleDefaultEndpoints ["weather", "other"]]
                        )

      it "fails if two endpoints have the same name" $ do
        parseConfiguration "/test" duplicateEndpoints
          `shouldThrow` (== InvalidConfig [DuplicateEndpointNames ["weather"]])

    context "themes section" $ do
      it "fails if two themes are marked as default" $ do
        parseConfiguration "/test" twoDefaultThemes
          `shouldThrow` (== InvalidConfig
                          [MultipleDefaultThemes ["default", "other"]]
                        )

      it "fails if two themes have the same name" $ do
        parseConfiguration "/test" duplicateThemes
          `shouldThrow` (== InvalidConfig [DuplicateThemeNames ["weather"]])

      context "theme path" $ do
        it "considers absolute path as absolute" $ do
          cfg <- parseConfiguration "/test" validConfiguration
          (cfg ^. appConfigDefaultTheme . themePath)
            `shouldBe` "/path/to/theme-file"

        it "considers relative as relative to directory of config file" $ do
          cfg <- parseConfiguration "/home/ben/.config/test.yaml"
                                    relativeThemePath
          (cfg ^. appConfigDefaultTheme . themePath)
            `shouldBe` "/home/ben/.config/theme-file"


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


relativeThemePath :: ByteString
relativeThemePath = [r|
endpoints:
  - name: weather
    default: true
    url: "https://example.com/api"

themes:
  - name: default
    default: true
    path: theme-file
|]

validConfiguration :: ByteString
validConfiguration = [r|
application:
  tickrate: 10000
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
