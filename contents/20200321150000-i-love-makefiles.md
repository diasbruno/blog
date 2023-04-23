Why aren't we using old stuff?
Is it just because it is...old?

`make` is great and we should use it more!

We are not going to talk in details about 
`make` (there are a lot of good articles
to read already), but to encorage 
its adoption to help make our lives easier.

Let me give you a scenario...

```sh
cd a_project
make
sudo make install
```

Did we just build and installed on our system
a `python` project? Or it was a `javascript` one?
Maybe it was a `C++` project that uses `CMake`...

This is `make`'s specialty...to execute tasks!

What are the benefits:

- Now, we don't need to remember those long commands (`make docker-run`)
- Anyone can build, install and run the project
- You can use it to write an entire pipeline to generate
changelogs, update versions, tagging,
without knowing which tools it uses under the hood.
