# elasticnt

[N-Triples](http://www.w3.org/TR/n-triples/) intake to elastic search


## Inspiration 

N-Triples is a text base, widely-supported protocol to exchange knowledge graphs. The library defines schema and rules to store knowledge graphs into Elastic Search.    


## Getting started

The latest version of the library is available at its master branch. All development, including new features and bug fixes, take place on the master branch using forking and pull requests as described in contribution guidelines.

To use and develop the library you need:
* Erlang/OTP 18.x or later
* rebar3
* Elastic Search

### Installation

If you use `rebar` you can include the library in your project with
```
{elasticnt, ".*",
   {git, "https://github.com/fogfish/elasticnt", {branch, master}}
}
```

### Running 

The library requires Elastic Search cluster, the docker container is easiest way to run standalone instance of the cluster for development purposes.  
```
docker run -it -p 9200:9200 fogfish/elasticsearch
```

Build library and run the development console
```
make
make run
```

Let's create an index and store the N-triple
```erlang
%%
%% start application
elasticnt:start().

%%
%% open cluster connection, define implicit index identity
{ok, Sock} = esio:socket("http://192.168.99.100:9200/nt").

%%
%% deploy index schema
elasticnt:schema(Sock, []).

%%
%% store N-triple to index
elasticnt:put(Sock, 
   #{
      s => <<"http://example.org">>,
      p => <<"dc:title">>,
      o => <<"Example Domain">>
   }
).
```

You can inspect created index and data using Elastic Search REST API .
```
curl 'http://192.168.99.100:9200/_search?q=Example&pretty'
```


### More Information
* study [native interface](src/elasticnt.erl)
* understand the design consideration of index [schema](src/schema.md)
* check [tips and hints](doc/howto.md)

## How to Contribute

`elasticnt` is Apache 2.0 licensed and accepts contributions via GitHub pull requests.

### getting started

* Fork the repository on GitHub
* Read the README.md for build instructions
* Make pull request

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>

## Bugs

If you detect a bug, please bring it to our attention via GitHub issues. Please make your report detailed and accurate so that we can identify and replicate the issues you experience:
- specify the configuration of your environment, including which operating system you're using and the versions of your runtime environments
- attach logs, screen shots and/or exceptions if possible
- briefly summarize the steps you took to resolve or reproduce the problem


## License

Copyright 2016 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

