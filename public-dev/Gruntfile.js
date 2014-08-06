'use strict';

module.exports = function (grunt) {

  require('time-grunt')(grunt);
  grunt.file.expand('./node_modules/grunt-*/tasks').forEach(grunt.loadTasks);

  function mountFolder (connect, dir, maxage) {
    return connect.static(require('path').resolve(grunt.template.process(dir)), { maxAge: maxage||0 });
  }

  grunt.initConfig({

    // Project settings
    yeoman: {
      // configurable paths
      app: require('./bower.json').appPath || './',
      dist: '../public'
    },

    // Watches files for changes and runs tasks based on the changed files
    watch: {
      options: {
        nospawn: true,
        livereload: '<%= connect.options.livereload %>'
      },
      js: {
        files: [
          '{.tmp,<%= yeoman.app %>}/scripts/{,*/}*.js',
          'test/mock/api.js',
          'test/mock/wix.js'
        ],
        tasks: ['newer:jshint:all']
      },
      css: {
        files: ['{.tmp,<%= yeoman.app %>}/styles/{,*/}*.css'],
        tasks: ['copy:styles', 'autoprefixer']
      },
      gruntfile: {
        files: ['Gruntfile.js']
      },
      livereload: {
        options: {
          livereload: true
        },
        files: [
          '<%= yeoman.app %>/{,*/}*.html',
          '.tmp/styles/{,*/}*.css',
          '<%= yeoman.app %>/images/{,*/}*.{png,jpg,jpeg,gif,webp,svg}'
        ]
      }
    },

    // The actual grunt server settings
    connect: {
      options: {
        hostname: 'localhost',
        livereload: 35729
      },
      livereload: {
        options: {
          debug: false,
          open: true,
          port: 9000,
          middleware: function (connect) {
            return [
              mountFolder(connect, 'test', 86400000),
              mountFolder(connect, '.tmp', 86400000),
              mountFolder(connect, '<%= yeoman.app %>', 86400000)
            ];
          }
        }
      },
      test: {
        options: {
          port: 9000,
          middleware: function (connect) {
            return [
              mountFolder(connect, 'test', 86400000),
              mountFolder(connect, '<%= yeoman.dist %>', 86400000)
            ];
          }
        }
      },
      dist: {
        options: {
          base: '<%= yeoman.dist %>'
        }
      }
    },

    // Make sure code styles are up to par and there are no obvious mistakes
    jshint: {
      options: {
        jshintrc: '.jshintrc',
        reporter: require('jshint-stylish')
      },
      all: [
        'Gruntfile.js',
        '<%= yeoman.app %>/scripts/{,*/}*.js'
      ],
      test: {
        options: {
          jshintrc: 'test/.jshintrc'
        },
        src: ['test/spec/{,*/}*.js']
      }
    },


    // Empties folders to start fresh
    clean: {
      dist: {
        files: [
          {
            dot: true,
            src: [
              '.tmp',
              '<%= yeoman.dist %>/*',
              '!<%= yeoman.dist %>/.git*'
            ]
          }
        ],
        options: {
          force: true
        }
      },
      server: '.tmp'
    },

    // Add vendor prefixed styles
    autoprefixer: {
      options: {
        browsers: ['> 1%']
      },
      dist: {
        files: [
          {
            expand: true,
            cwd: '.tmp/styles/',
            src: '{,*/}*.css',
            dest: '.tmp/styles/'
          }
        ]
      }
    },

    rev: {
      dist: {
        files: {
          src: [
            '<%= yeoman.dist %>/scripts/{,*/}*.js',
            '<%= yeoman.dist %>/styles/{,*/}*.css',
            '<%= yeoman.dist %>/images/{,*/}*.{png,jpg,jpeg,gif,webp,svg}',
          ]
        }
      }
    },


    // Reads HTML for usemin blocks to enable smart builds that automatically
    // concat, minify and revision files. Creates configurations in memory so
    // additional tasks can operate on them
    useminPrepare: {
      html: ['<%= yeoman.app %>/index.html', '<%= yeoman.app %>/settings.html', '<%= yeoman.app %>/moderating.html'],
      options: {
        dest: '<%= yeoman.dist %>'
      }
    },

    // Performs rewrites based on rev and the useminPrepare configuration
    usemin: {
      html: ['<%= yeoman.dist %>/{,*/}*.html'],
      css: ['<%= yeoman.dist %>/styles/{,*/}*.css'],
      options: {
        assetsDirs: ['<%= yeoman.dist %>']
      }
    },

    // The following *-min tasks produce minified files in the dist folder
    htmlmin: {
      dist: {
        options: {
          // Optional configurations that you can uncomment to use
          // removeCommentsFromCDATA: true,
          // collapseBooleanAttributes: true,
          // removeAttributeQuotes: true,
          // removeRedundantAttributes: true,
          // useShortDoctype: true,
          // removeEmptyAttributes: true,
          // removeOptionalTags: true*/
        },
        files: [
          {
            expand: true,
            cwd: '<%= yeoman.app %>',
            src: ['*.html', 'views/*.html'],
            dest: '<%= yeoman.dist %>'
          }
        ]
      }
    },

    // Allow the use of non-minsafe AngularJS files. Automatically makes it
    // minsafe compatible so Uglify does not destroy the ng references
    ngmin: {
      dist: {
        files: [
          {
            expand: true,
            cwd: '.tmp/concat/scripts',
            src: '*.js',
            dest: '.tmp/concat/scripts'
          }
        ]
      }
    },

    // Replace Google CDN references
    cdnify: {
      dist: {
        html: ['<%= yeoman.dist %>/*.html']
      }
    },


//     cssmin: {
//       minify: {
//         expand: true,
//         cwd: '<%= yeoman.app %>/styles/',
//         src: ['*.css', '!*.min.css'],
//         dest: '<%= yeoman.dist %>/styles/',
//         ext: '.min.css'
//       }
//     },

    // Copies remaining files to places other tasks can use
    copy: {
      dist: {
        files: [
          {
            expand: true,
            dot: true,
            cwd: '<%= yeoman.app %>',
            dest: '<%= yeoman.dist %>',
            src: [
              '*.html', 'views/**/*.html',
              '*.{ico,png,txt}',
              '.htaccess',
              'bower_components/**/*',
              'images/{,*/}*.{webp,ico,png,jpg,jpeg,gif,svg}',
              'styles/fonts/*',
              'translations/*'
            ]
          },
          {
            expand: true,
            cwd: '.tmp/images',
            dest: '<%= yeoman.dist %>/images',
            src: [
              'generated/*'
            ]
          }
        ]
      },
      styles: {
        expand: true,
        cwd: '<%= yeoman.app %>/.tmp/concat/styles',
        dest: '<%= yeoman.dist %>/styles/',
        src: '{,*/}*.css'
      }
    },

    bower: {
      install: {
        options: {
          targetDir: '<%= yeoman.dist %>/bower_components',
          install: true,
          verbose: false,
          cleanTargetDir: false,
          cleanBowerDir: false
        }
      }
    },

    // Test settings
    karma: {
      unit: {
        configFile: 'test/karma.conf.js',
        singleRun: false
      },
      e2e: {
        proxies: { '/': 'http://localhost:9000/' },
        configFile: 'test/karma-e2e.conf.js',
        browsers: ['Chrome'],
        autoWatch: true,
        singleRun: false
      }
    },

    processhtml: {
      options: {
        commentMarker: 'process'
      },
      dist: {
        files: [{
          expand: true,
          src: '<%= yeoman.dist %>/*.html',
          dest: '.'
        }]
      }
    }
  });


  grunt.registerTask('serve', function () {
    grunt.task.run([
      'copy:styles',
      'autoprefixer',
      'connect:livereload',
      'watch'
    ]);
  });

  grunt.registerTask('test', function () {
    grunt.task.run([
      'copy:styles',
      'autoprefixer',
      'connect:test',
      'karma:unit'
    ]);
  });

  grunt.registerTask('build', [
    'clean:dist',
    'bower:install',
    'useminPrepare',
    'autoprefixer',
    'concat',
    'ngmin',
    'copy:styles',
    'copy:dist',
    'processhtml:dist',
    'cdnify',
//     'cssmin',
    'uglify',
    'rev',
    'usemin'
  ]);
};
