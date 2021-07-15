# RestRserve

<details>

* Version: 0.4.1
* GitHub: https://github.com/rexyai/RestRserve
* Source code: https://github.com/cran/RestRserve
* Date/Publication: 2021-01-04 21:20:25 UTC
* Number of recursive dependencies: 63

Run `revdep_details(, "RestRserve")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/tinytest.R’ failed.
    Last 13 lines of output:
      test-cl-application.R.........   49 tests [0;32mOK[0m 
      test-cl-application.R.........   49 tests [0;32mOK[0m 
      test-cl-application.R.........   50 tests [0;32mOK[0m 
      test-cl-application.R.........   50 tests [0;32mOK[0m 
      test-cl-application.R.........   50 tests [0;32mOK[0m 
      test-cl-application.R.........   51 tests [0;32mOK[0m 
      test-cl-application.R.........   52 tests [0;32mOK[0m 
      test-cl-application.R.........   52 tests [0;32mOK[0m 
      test-cl-application.R.........   52 tests [0;32mOK[0m 
      test-cl-application.R.........   52 tests [0;32mOK[0m 
      test-cl-application.R.........   52 tests [0;32mOK[0m 
      test-cl-application.R.........   52 tests [0;32mOK[0m Error in curl::curl_fetch_memory("http://localhost:1234/status") : 
        Failed to connect to localhost port 1234: Connection refused
      Calls: <Anonymous> ... FUN -> eval -> eval -> do_test_external -> <Anonymous>
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘Rserve’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

