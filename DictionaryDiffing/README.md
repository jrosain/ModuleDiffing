## Dictionary Diffing

We have implemented a dictionary diffing which consists of
- dictionary diffing is like tree diffing but each "layer" is diffed independently
    - dictionary can have dictionary in values
- we check all keys in d1
- if the key is not in d2
    - add a deletion in the patch
- if the key is in d2
    - check the values
    - if values ar not the same
        - add a modification in the patch for this key
- we check all keys in d2
- if the key is not in d1
    - add an insertion in the patch