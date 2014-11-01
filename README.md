bootstrap-csh
=============

CSS overrides for [Twitter Bootstrap](http://getbootstrap.com), for use in [Computer Science House](http://csh.rit.edu) websites. 

Includes two themes:
* `public` - The "Hacker" theme (green), for CSH public sites
* `members` - The "PDP-11" theme (purple and pink), for internal CSH sites

Minified files can be found in the `release` directory. Original files with comments can be found in the `dev` directory.

Using Grunt
-----------

First install grunt-cli: `npm install -g grunt-cli`

Next, install dependencies: `npm install`

To minify CSS files: `grunt cssmin`, or just `grunt`