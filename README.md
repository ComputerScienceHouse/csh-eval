bootstrap-csh
=============

CSS overrides for [Twitter Bootstrap](http://getbootstrap.com), for use in [Computer Science House](http://csh.rit.edu) websites. 

Includes two themes:
* `public` - The "Hacker" theme (green), for CSH public sites
* `members` - The "PDP-11" theme (purple and pink), for internal CSH sites

Minified and compiled files can be found in the `release` directory. Original development files can be found in the `dev` directory. Example pages using both themes can be found in the `test` directory.

Contributing
------------

To contribute to this project:
* Fork the repository.
* Make your changes to `members.less` and/or `public.less` in the "dev" directory.
* Compile and minify your changes (see "Using Grunt" below).
* Verify your changes on the test pages (`test/members/` or `test/public/`).
* Push your changes and submit a pull request.

Using Grunt
-----------

First install grunt-cli: `npm install -g grunt-cli`

Next, install dependencies: `npm install`

To compile and minify all LESS source files: `grunt` or `grunt default`

To only compile LESS to CSS:
* `grunt less:members` or `grunt less:public` to minify individual files
* `grunt less` to compile all files

To only minify CSS files: 
* `grunt cssmin:members` or `grunt cssmin:public` to minify individual files
* `grunt cssmin` to minify all files
