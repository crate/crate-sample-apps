'use strict';

angular.module('app', [])
  .value('apiHost', 'http://localhost:8080')
  .service('apiUrl', function(apiHost) {
    return function(path) {
      return apiHost + path;
    }
  })
  .service('Api', function(apiUrl, $q, $http) {
    return function(path) {
      var url = apiUrl(path);
      this.post = function(data) {
        // CREATE
        return $http.post(url, JSON.stringify(data));
      };
      this.get = function() {
        // RETRIEVE
        return $http.get(url);
      };
      this.put = function(data) {
        // UPDATE
        return $http.put(url, JSON.stringify(data));
      };
      this.delete = function() {
        // DELETE
        return $http.delete(url);
      };
    }
  })
  .service('Location', function($q) {
    var getLocation = function(success, error) {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(success);
      } else {
        error(new Error('Your browser does not support HTML5 location.'));
      }
    };
    return function() {
      this.get = function() {
        var d = $q.defer();
        getLocation(function(position){
          d.resolve(position);
        }, function(e){
          d.reject(e);
        });
        return d.promise;
      };
    }
  })
  .service('objectArrayIndexOf', function() {
    return function(arr, k, v) {
      // usage: objectArrayIndexOf([{id:2},{id:1},{id:3}], 'id', 1) -> 1
      for (var i = 0; i < arr.length; i++) {
        if (arr[i][k] === v) return i;
      }
      return -1;
    };
  })
  .directive("blob", function() {
    return {
      scope: {
        blob: "="
      },
      link: function($scope, elem, attr) {
        elem.bind("change", function(changeEvent) {
          var reader = new FileReader();
          reader.onload = function(loadEvent) {
            $scope.$apply(function() {
              $scope.blob = loadEvent.target.result;
            });
          }
          reader.readAsBinaryString(changeEvent.target.files[0]);
        });
      }
    }
  })
  .controller('GuestbookController', function($scope, $q, apiHost, Api, Location, objectArrayIndexOf) {

    var EMPTY = {
      user: {
        name: null,
        location: null
      },
      text: null,
      image_ref: null
    };
    var locationCache = null;
    var SEARCH_API = new Api('/search');

    $scope.posts = [];
    $scope.imagedata = null;
    $scope.formdata = angular.copy(EMPTY);
    $scope.apiHost = apiHost;
    $scope.search = {
      query_string: ''
    };

    var init = function() {
      // try to get location from browser
      var location = new Location();
      location.get().then(function(pos) {
        locationCache = [pos.coords.longitude, pos.coords.latitude];
        $scope.formdata.user.location = angular.copy(locationCache);
      }, function(e) {
        console.warn(e);
        window.alert(e.message);
      });
      // load existing posts
      loadPosts();
    };

    var loadPosts = function() {
      var api = new Api('/posts');
      api.get().then(function(response) {
        $scope.posts = response.data;
      }, function(e) {
        console.warn(e);
        $scope.posts = [];
      });
    };

    // reset input form but refill location from cache
    var resetForm = function() {
      $scope.formdata = angular.copy(EMPTY);
      $scope.formdata.user.location = angular.copy(locationCache);
      $scope.imagedata = null;
    };

    var uploadBlob = function(blob) {
      var d = $q.defer();
      var api = new Api('/images');
      api.post({'blob': btoa(blob)}).then(function(response) {
        d.resolve(response);
      }, function(response) {
        if (response.status === 409) {
          d.resolve(response);
        } else {
          d.reject(response);
        }
      })
      return d.promise;
    };

    var submitPost = function() {
      var api = new Api('/posts');
      api.post($scope.formdata).then(function(response) {
        var posts = response.data;
        for (var i=0; i<posts.length; i++) {
          $scope.posts.unshift(posts[0]);
        }
        resetForm();
      }, function(e) {
        console.warn(e);
        window.alert('Creating the post failed.');
      })
    };

    var searchPosts = function() {
      SEARCH_API.post($scope.search).then(function(response) {
        $scope.posts = response.data;
      }, function(e) {
        console.warn(e);
        $scope.posts = [];
      });
    };

    // watch search input
    $scope.$watch(function(scope) {
      return scope.search.query_string;
    }, function(newVal, oldVal) {
      if (newVal === '') {
        loadPosts();
      } else if (newVal != oldVal) {
        searchPosts();
      }
    });


    // create new post
    this.submitForm = function() {
      if (!$scope.formdata.user.location) return;
      if ($scope.imagedata) {
        uploadBlob($scope.imagedata).then(function(response) {
          $scope.formdata.image_ref = response.data.digest;
          submitPost();
        }, function(e) {
          console.warn(e);
          window.alert('Image upload failed.');
        });
      } else {
        submitPost();
      }
    };

    // like an existing post
    this.likePost = function(post) {
      var api = new Api('/post/' + post.id + '/like');
      api.put().then(function(response) {
        post.like_count = response.data.like_count;
      }, function(e) {
        console.warn(e);
        window.alert('Liking the post failed.');
      });
    };

    // edit existing post
    this.editPost = function(post) {
      console.warn('editPost() is not implemented');
    };

    // delete existing post
    this.deletePost = function(post) {
      var idx = objectArrayIndexOf($scope.posts, 'id', post.id);
      var api = new Api('/post/' + post.id);
      api.delete().then(function(response) {
        $scope.posts.splice(idx, 1);
      }, function(e){
        console.warn(e);
        window.alert('Deleting the post failed.');
      });
    };

    // clear search form
    this.clearSearch = function() {
      $scope.search = {
        query_string: ''
      };
    };

    // initialize controller
    init();

  });
