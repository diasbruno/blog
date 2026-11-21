I love [Comby](https://github.com/comby-tools/comby), in their words:

> "A tool for structural code search and replace
that supports ~every language."

Most of the time when I'm reviewing some pull request,
I always have the same thoughts:

- "I've seem this before..."
- "I see a pattern here..."

And then I:

- Go to the project folder.
- Update the repository.
- Go to the PR's branch.
- Figure out a `grep` that can extract what I want.

As we all know, step 4 is the one that takes a lot of time.

`comby` understands a little bit of syntax and deals well
balancing `()`, `[]` and `{}`,
which is the most painful thing to do when writing regex.

Example:

After some interactions on our software, we had many methods related to permissions written on views
and components. Eventually, things starts getting duplicated, so it was time to refactor/reorganize.

Using comby, this was basically (or close) of what I had to type:

```
comby 'can:[~[A-Za-z0-9_]*](:[h]) {:[i]}' '' -matcher .js
```

Output example:

```
------ file.vue
++++++ file.vue
@|-1,5 +1,3 ============================================================
 |  computed: {
!|     canEdit() {
!|       return this.user.permissions.contains(Permissions.X);
!|     }
 |  }
```

This expression highlights every function/method
that is prefixed with 'can' (`:[h]` and `:[I]`
are just placeholders saying "whatever is in between
the parenthesis and curly brackets").

With the result in hand, I show to the team, we discussed
when we could tackle this issue, we had some ideas
and we start the refactor.

This is one use case of comby, go checkout their page
and I hope this will be your favorite tool as it's mine now.
