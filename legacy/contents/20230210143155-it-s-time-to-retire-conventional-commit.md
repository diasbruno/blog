This commit style is extremely popular.
It tries to help to organize
a project's version control history.

It probably still have some application
or where it can used.

Unfortunately, it doesn't help to solve
an important issue when you need to analyze
and maintain a project.

It's about identifying and grouping related
commits.

It's kind silly saying it out loud,
but this is something that was overlooked
mostly because there are very few people
doing serious hardcore git commands
or analyzing repositories.

After doing some code and repository analyses,
the most annoying thing to do is relating commits
with external tools.

It can take a really long time
and it's annoying because it should
be easy to automate.

Here is an example:

`E-1` is an epic and `T-1` e `T-2` are tasks of this epic.
If you want to find E-1, you can search for T-1 and T-2,
or the epic itself if you create the epic branch.

```
hash commit
5555 feat: added page for resource
4444 refator: other peoples work
3333 feat: added service for resource
2222 fix: other peoples work
1111 feat: added http stuff for resource
0000 ....
```

We know `1111`, `3333` and `5555` are related,
but it's hard to automate, so we have to do it
manually.

Also, when quering a repository,
these tags have pretty much no use.

If you have a failure in the process,
a feature gets to production
and the team decide that it will be the best
to revert the whole thing, it can be very challenge
because more and more stuff will be pushed
to the master/main branch.

Using the identifier of your project management system
is a simple way an have a lot of benefits when you need
to query the repository.

This is not a new thing. This style is out there as well...

```
hash commit
5555 T-2 | added page for resource
4444 T-Y | other peoples work
3333 T-1 | added service for resource
2222 T-X | other peoples work
1111 T-1 | added http stuff for resource
0000 ....
```

With an external identifier, it help to query the history
in a very efficient way and it become very shell friendly.

Here is a command to query an epic `E-1`:

git log --oneline --grep="^T-[12]"

```
hash commit
5555 T-2 | added page for resource
3333 T-1 | added service for resource
1111 T-1 | added http stuff for resource
```

With query you can easily extract a .diff
to revert an entire feature without suffering.
