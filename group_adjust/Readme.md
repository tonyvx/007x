---
title: group_adjust.py
author: tonyvx
---
**group_adjust.py**

##
*group_adjust calls 1 of either two methods:*
  
  _group_adjust_panda_ : uses pandas' dataframe to load data and computed demeand list
  
  _group_adjust_python_: uses pure python to generate a mean for each uniq element in groups and then computes weighted mean using weights

```python
  if isPanda:
        return group_adjust_panda(vals, groups, weights)
  else:
        return group_adjust_python(groups, vals, weights)
```

* In the main logger is instantiated
* Log level is set to ERROR for best performance. Available INFO, DEBUG
* isPanda is set to True to use pandas library. 
  * pandas - test_performance took 10-12 secs with 9-10 secs in creating data frame
  * python - test_performance took 17-20 secs with bulk of the time spent in computing weighted mean

```python
if __name__ == '__main__':
    FORMAT = '%(asctime)-15s : %(levelname)s : %(name)s : %(message)s'
    log.basicConfig(format=FORMAT)

    l=log.getLogger('group_adjust')
    l.setLevel(log.ERROR)

    isPanda = True
    unittest.main()
```    

**unittests**

assertion was added to expect ValueError for

* test_weights_len_equals_group_len

* test_group_len_equals_vals_len

```python
        with self.assertRaises(ValueError) as context:
            adj_vals = group_adjust(vals, [grps_1, grps_2, grps_3], weights)
```

