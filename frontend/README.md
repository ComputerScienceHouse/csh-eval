# CSH Evaluations Database Frontend

This is where the hamlet and static assets will go for the frontend. The
actual organization on the filesystem doesn't matter for the application, but
you should put your pages around where they go in the real route, for
convenience. 

The infrastructure is designed to minimize the amount of Haskell that needs
to be written or changed to modify the frontend. When changing pages without
changing the data that is needed for a page, no Haskell changes are needed.

## Changing a page

All of the pages in the frontend for the evaluations database are written in
the shakespearean templating language. Hamlet is the HTML template language for
shakespeare. A good introduction to the language is in the [Yesod Book chapter
on it][shakespeare]. 

If you are just changing layout or styling, you won't have to touch any
Haskell. 

## Adding a page

Remember to follow the contributing guidelines found on the main readme.

This guide for adding pages is meant for basic pages. Ones with more complex
layouts (dashboards, for instance) will be more difficult, but hopefully won't
have to be added very often. 

1.  Add a hamlet page under the `frontend` folder. Make sure it is named well
    and placed under it's proper category (`voting/`, `packet/`, `evals/`,
    etc.).

2.  Add the route to your new page in the `src/CSH/Eval/Frontend.hs` file under
    the route definitions. The layout of the route definition is

        /route/to/page NameOfRouteR GET

    The name of the route should have something do to with the route itself, 
    since all pages have names, and we want page names to be unambiguous. 

    If the route to the page needs arguments, see the [Yesod Routing][routing] 
    page for more details.

3.  Add the handler for the page at the bottom of the `src/CSH/Eval/Frontend.hs`
    file. The handler is defined to be named
    ``` Haskell
        getNameOfRouteR :: Handler Html
    ```
    for the above route example. 

    The handler should be defined as
    ```Haskell
        getNameOfRouteR = defaultLayout $(whamletFile "frontend/folder/folder/file.hamlet")
    ```
4.  Make your page. See the [Yesod Shakespeare Templates][shakespeare] page for
    details on making pages using the hamlet templating language.

## Calling the API from your page

It is easy to call the API from your page. There are two ways to do it. 

If you need to call the API from your template while the page is being
generated, you are going to use the Haskell API. The Haskell names for all of
the API calls are listed on the API specification. Just use it like any other
function. To see how to call a function in Hamlet, look at the [Yesod
Shakespeare Templates][shakespeare] page.

If you need to call the API from javascript, we provide that funcitonality
too! Just call the function listed in the API specification for Javascript
and it will handle the ajax call. Provide a callback, and you are all set with
your data in hand!

## Questions

If you are ever trying to contribute but can't figure something out, you can
check out the exsiting pages for examples, read the [Yesod Book chapter on
Shakespeare][shakespeare], or talk to one of the main developers for help.

[routing]: http://www.yesodweb.com/book/routing-and-handlers
[shakespeare]: http://www.yesodweb.com/book/shakespearean-templates
