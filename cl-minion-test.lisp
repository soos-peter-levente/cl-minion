(defpackage :cl-minion-test
  (:use :common-lisp :fiveam :cl-minion)
  (:nicknames :minion-test)
  (:export :run-test))

(in-package :cl-minion-test)

(def-suite cl-minion-test
  :description "Regression testing suite for Minion JSON parser.")

(test cl-minion-test
  (is (parse "{\"hello\":null}")
      '(:OBJ ((:KEY "hello" :NUL NIL))))
  (is (parse "{\"hello\":\"world\"}")
      '(:OBJ ((:KEY "hello" :STR "world"))))
  (is (parse "{\"hello\":{\"nested\":\"json\"}}")
      '(:OBJ ((:KEY "hello" :OBJ ((:KEY "nested" :STR "json"))))))
  (is (parse "{\"hello\":{\"integer\":123}}")
      '(:OBJ ((:KEY "hello" :OBJ ((:KEY "integer" :NUM 123))))))
  (is (parse "{\"hello\":{\"float\":123}}")
      '(:OBJ ((:KEY "hello" :OBJ ((:KEY "float" :NUM 123.23))))))
  (is (parse "[{\"hello\":\"world\",\"how\":\"are\",\"you\":\"today\"}]")
      '(:ARR ((:OBJ ((:KEY "hello" :STR "world") (:KEY "how" :STR "are")
                     (:KEY "you" :STR "today"))))))
  (is (parse "[true,false,false,null,true]")
      '(:ARR ((:TRU T) (:FAL NIL) (:FAL NIL) (:NUL NIL) (:TRU T))))
  (is (type-of (parse json-1)) 'CONS)
  (is (type-of (parse json-2)) 'CONS)
  (is (type-of (parse json-3)) 'CONS))

(defparameter json-1
  "{
  \"glossary\": {
    \"title\": \"example glossary\",
    \"GlossDiv\": {
      \"title\": \"S\",
      \"GlossList\": {
        \"GlossEntry\": {
          \"ID\": \"SGML\",
          \"SortAs\": \"SGML\",
          \"GlossTerm\": \"Standard Generalized Markup Language\",
          \"Acronym\": \"SGML\",
          \"Abbrev\": \"ISO 8879:1986\",
          \"GlossDef\": {
            \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
            \"GlossSeeAlso\": [
              \"GML\",
              \"XML\"
            ]
          },
          \"GlossSee\": \"markup\"
        }
      }
    }
  }
}")

(defparameter json-2
  "{\"menu\": {
    \"header\": \"SVG Viewer\",
    \"items\": [
        {\"id\": \"Open\"},
        {\"id\": \"OpenNew\", \"label\": \"Open New\"},
        null,
        {\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},
        {\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},
        {\"id\": \"OriginalView\", \"label\": \"Original View\"},
        null,
        {\"id\": \"Quality\"},
        {\"id\": \"Pause\"},
        {\"id\": \"Mute\"},
        null,
        {\"id\": \"Find\", \"label\": \"Find...\"},
        {\"id\": \"FindAgain\", \"label\": \"Find Again\"},
        {\"id\": \"Copy\"},
        {\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},
        {\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},
        {\"id\": \"ViewSVG\", \"label\": \"View SVG\"},
        {\"id\": \"ViewSource\", \"label\": \"View Source\"},
        {\"id\": \"SaveAs\", \"label\": \"Save As\"},
        null,
        {\"id\": \"Help\"},
        {\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}
    ]
}}")

(defparameter json-3
  "{
  \"web-app\": {
    \"servlet\": [
      {
        \"servlet-name\": \"cofaxCDS\",
        \"servlet-class\": \"org.cofax.cds.CDSServlet\",
        \"init-param\": {
          \"configGlossary:installationAt\": \"Philadelphia, PA\",
          \"configGlossary:adminEmail\": \"ksm@pobox.com\",
          \"configGlossary:poweredBy\": \"Cofax\",
          \"configGlossary:poweredByIcon\": \"/images/cofax.gif\",
          \"configGlossary:staticPath\": \"/content/static\",
          \"templateProcessorClass\": \"org.cofax.WysiwygTemplate\",
          \"templateLoaderClass\": \"org.cofax.FilesTemplateLoader\",
          \"templatePath\": \"templates\",
          \"templateOverridePath\": \"\",
          \"defaultListTemplate\": \"listTemplate.htm\",
          \"defaultFileTemplate\": \"articleTemplate.htm\",
          \"useJSP\": false,
          \"jspListTemplate\": \"listTemplate.jsp\",
          \"jspFileTemplate\": \"articleTemplate.jsp\",
          \"cachePackageTagsTrack\": 200,
          \"cachePackageTagsStore\": 200,
          \"cachePackageTagsRefresh\": 60,
          \"cacheTemplatesTrack\": 100,
          \"cacheTemplatesStore\": 50,
          \"cacheTemplatesRefresh\": 15,
          \"cachePagesTrack\": 200,
          \"cachePagesStore\": 100,
          \"cachePagesRefresh\": 10,
          \"cachePagesDirtyRead\": 10,
          \"searchEngineListTemplate\": \"forSearchEnginesList.htm\",
          \"searchEngineFileTemplate\": \"forSearchEngines.htm\",
          \"searchEngineRobotsDb\": \"WEB-INF/robots.db\",
          \"useDataStore\": true,
          \"dataStoreClass\": \"org.cofax.SqlDataStore\",
          \"redirectionClass\": \"org.cofax.SqlRedirection\",
          \"dataStoreName\": \"cofax\",
          \"dataStoreDriver\": \"com.microsoft.jdbc.sqlserver.SQLServerDriver\",
          \"dataStoreUrl\": \"jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon\",
          \"dataStoreUser\": \"sa\",
          \"dataStorePassword\": \"dataStoreTestQuery\",
          \"dataStoreTestQuery\": \"SET NOCOUNT ON;select test='test';\",
          \"dataStoreLogFile\": \"/usr/local/tomcat/logs/datastore.log\",
          \"dataStoreInitConns\": 10,
          \"dataStoreMaxConns\": 100,
          \"dataStoreConnUsageLimit\": 100,
          \"dataStoreLogLevel\": \"debug\",
          \"maxUrlLength\": 500
        }
      },
      {
        \"servlet-name\": \"cofaxEmail\",
        \"servlet-class\": \"org.cofax.cds.EmailServlet\",
        \"init-param\": {
          \"mailHost\": \"mail1\",
          \"mailHostOverride\": \"mail2\"
        }
      },
      {
        \"servlet-name\": \"cofaxAdmin\",
        \"servlet-class\": \"org.cofax.cds.AdminServlet\"
      },
      {
        \"servlet-name\": \"fileServlet\",
        \"servlet-class\": \"org.cofax.cds.FileServlet\"
      },
      {
        \"servlet-name\": \"cofaxTools\",
        \"servlet-class\": \"org.cofax.cms.CofaxToolsServlet\",
        \"init-param\": {
          \"templatePath\": \"toolstemplates/\",
          \"log\": 1,
          \"logLocation\": \"/usr/local/tomcat/logs/CofaxTools.log\",
          \"logMaxSize\": \"\",
          \"dataLog\": 1,
          \"dataLogLocation\": \"/usr/local/tomcat/logs/dataLog.log\",
          \"dataLogMaxSize\": \"\",
          \"removePageCache\": \"/content/admin/remove?cache=pages&id=\",
          \"removeTemplateCache\": \"/content/admin/remove?cache=templates&id=\",
          \"fileTransferFolder\": \"/usr/local/tomcat/webapps/content/fileTransferFolder\",
          \"lookInContext\": 1,
          \"adminGroupID\": 4,
          \"betaServer\": true
        }
      }
    ],
    \"servlet-mapping\": {
      \"cofaxCDS\": \"/\",
      \"cofaxEmail\": \"/cofaxutil/aemail/*\",
      \"cofaxAdmin\": \"/admin/*\",
      \"fileServlet\": \"/static/*\",
      \"cofaxTools\": \"/tools/*\"
    },
    \"taglib\": {
      \"taglib-uri\": \"cofax.tld\",
      \"taglib-location\": \"/WEB-INF/tlds/cofax.tld\"
    }
  }
}")

(defun run-test () (run! 'cl-minion-test))
