## R CMD check results

0 errors | 0 warnings | 0 notes

### 2.2.2 -> 2.2.3

This patch addresses the following violation:

'Packages which use Internet resources should fail gracefully with an informative message
if the resource is not available or has changed (and not give a check warning nor error).'

I have fixed the relevant functions: `import_insightmaker()`, `url_to_insightmaker()`, and `insightmaker_to_json()`.



