/*
* Gruntfile for bootstrap-csh
*/
module.exports = function(grunt) {

// Project config
grunt.initConfig({
  pkg: grunt.file.readJSON('package.json'),
  cssmin: {
    members: {
      options: {
        banner: '/*! <%= pkg.name %>/members.min.css, v<%= pkg.version %>, minified <%= grunt.template.today("yyyy-mm-dd") %> */'
      },
      files: [{
        expand: true,
        cwd: 'dev/',
        src: ['members.css'],
        dest: 'release/',
        ext: '.min.css'
      }]
    },
    public: {
      options: {
        banner: '/*! <%= pkg.name %>/public.min.css, v<%= pkg.version %>, minified <%= grunt.template.today("yyyy-mm-dd") %> */'
      },
      files: [{
        expand: true,
        cwd: 'dev/',
        src: ['public.css'],
        dest: 'release/',
        ext: '.min.css'
      }]
    }
  },
  less: {
    members: {
      options: {
        paths: ["dev/"]
      },
      files: {
        "dev/members.css": "dev/members.less"
      }
    },
    public: {
      options: {
        paths: ["dev/"]
      },
      files: {
        "dev/public.css": "dev/public.less"
      }
    }
  }
});

// Load plugins
grunt.loadNpmTasks('grunt-contrib-cssmin');
grunt.loadNpmTasks('grunt-contrib-less');

// Register tasks
grunt.registerTask('default', ['less', 'cssmin']);

};