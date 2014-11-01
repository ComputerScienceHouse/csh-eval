/*
* Gruntfile for bootstrap-csh
*/
module.exports = function(grunt) {

// Project config
grunt.initConfig({
  pkg: grunt.file.readJSON('package.json'),
  cssmin: {
    options: {

    },
    my_target: {
      files: [{
        expand: true,
        cwd: 'dev/',
        src: ['*.css'],
        dest: 'release/',
        ext: '.min.css'
      }]
    }
  }
});

// Load plugins
grunt.loadNpmTasks('grunt-contrib-cssmin');

// Register tasks
grunt.registerTask('default', ['cssmin']);

};