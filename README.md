# Work ( Thinking of a better name... )

An app to manage my work across a bunch of apps

Starting with github

## Plan

### MVP
- [ ] Setup graphql requests
- [ ] Get all active pull requests, related to me
- [ ] All for creating todos on the PR
- [ ] Allow sharing and syncing of todos within the PR
- [ ] Create a note taking system that builds on the PR

### Next step
- [ ] Sync with local branches
- [ ] Allow for a more local workflow and then sync it with github PRs
- [ ] Come up with a better name

## Design Goals
- Fully keyboard driven
- Very simple UI
- Support offline

## Updating Graphql Schema

Github returns json through a simple curl
`curl -H "Authorization: bearer <token>" https://api.github.com/graphql > graphql_schema.json`

