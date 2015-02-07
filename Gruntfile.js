/*
* Gruntfile for bootstrap-csh
*/
module.exports = function(grunt) {

// Project config
grunt.initConfig({
  pkg: grunt.file.readJSON('package.json'),
  // Minify CSS
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
  // Compile LESS to CSS
  less: {
    members: {
      options: {
        paths: ['dev/']
      },
      files: {
        'dev/members.css': 'dev/members.less'
      }
    },
    public: {
      options: {
        paths: ['dev/']
      },
      files: {
        'dev/public.css': 'dev/public.less'
      }
    }
  },
  // Watch for changes
  watch: {
    members: {
      files: [ 'dev/members.less' ],
      tasks: [ 'less:members', 'cssmin:members' ]
    },
    public: {
      files: [ 'dev/public.less' ],
      tasks: [ 'less:public', 'cssmin:public' ]
    }
  },
  // Run a local server
  connect: {
    default: {
      options: {
        port: 9000,
        keepalive: true,
        base: './',
        hostname: '*'
      }
    },
    members: {
      options: {
        port: 9000,
        keepalive: true,
        base: './',
        hostname: '*',
        open: {
          target: 'http://localhost:9000/test/members'
        }
      }
    },
    public: {
      options: {
        port: 9000,
        keepalive: true,
        base: './',
        hostname: '*',
        open: {
          target: 'http://localhost:9000/test/public'
        }
      }
    }
  }
});

// Load plugins
grunt.loadNpmTasks('grunt-contrib-cssmin');
grunt.loadNpmTasks('grunt-contrib-less');
grunt.loadNpmTasks('grunt-contrib-connect');
grunt.loadNpmTasks('grunt-contrib-watch');

// Register tasks
grunt.registerTask('default', ['less', 'cssmin']);
grunt.registerTask('defaultMembers', ['less:members', 'cssmin:members']);
grunt.registerTask('defaultPublic', ['less:public', 'cssmin:public']);
grunt.registerTask('dev', ['default', 'watch']);
grunt.registerTask('devMembers', ['defaultMembers', 'watch:members']);
grunt.registerTask('devPublic', ['defaultPublic', 'watch:public']);
grunt.registerTask('test', ['connect:default']);
grunt.registerTask('testMembers', ['connect:members']);
grunt.registerTask('testPublic', ['connect:public']);

};