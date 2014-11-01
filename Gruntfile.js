/*
* Gruntfile for bootstrap-csh
*/
module.exports = function(grunt) {

// Project config
grunt.initConfig({
  pkg: grunt.file.readJSON('package.json'),
  cssmin: {
    members: {
      files: [{
        expand: true,
        cwd: 'dev/',
        src: ['members.css'],
        dest: 'release/',
        ext: '.min.css'
      }]
    },
    public: {
      files: [{
        expand: true,
        cwd: 'dev/',
        src: ['public.css'],
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