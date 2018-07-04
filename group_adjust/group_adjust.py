import unittest
import numpy as np
import pandas as pd
import logging as log
from datetime import datetime

# Your task is to write the group adjustment method below. There are some
# unimplemented unit_tests at the bottom which also need implementation.
# Your solution can be pure python, pure NumPy, pure Pandas
# or any combination of the three.  There are multiple ways of solving this
# problem, use comments to explain your code.

# Group Adjust Method
# The algorithm needs to do the following:
# 1.) For each group-list provided, calculate the means of the values for each
#     unique group.
#
#   For example:
#   vals       = [  1  ,   2  ,   3  ]
#   ctry_grp   = ['USA', 'USA', 'USA']
#   state_grp  = ['MA' , 'MA' ,  'CT' ]
#
#   There is only 1 country in the ctry_grp list.  So to get the means:
#     USA_mean == mean(vals) == 2
#     ctry_means = [2, 2, 2]
#   There are 2 states, so to get the means for each state:
#     MA_mean == mean(vals[0], vals[1]) == 1.5
#     CT_mean == mean(vals[2]) == 3
#     state_means = [1.5, 1.5, 3]
#
# 2.) Using the weights, calculate a weighted average of those group means
#   Continuing from our example:
#   weights = [.35, .65]
#   35% weighted on country, 65% weighted on state
#   ctry_means  = [2  , 2  , 2]
#   state_means = [1.5, 1.5, 3]
#   weighted_means = [2*.35 + .65*1.5, 2*.35 + .65*1.5, 2*.35 + .65*3]
#
# 3.) Subtract the weighted average group means from each original value
#   Continuing from our example:
#   val[0] = 1
#   ctry[0] = 'USA' --> 'USA' mean == 2, ctry weight = .35
#   state[0] = 'MA' --> 'MA'  mean == 1.5, state weight = .65
#   weighted_mean = 2*.35 + .65*1.5 = 1.675
#   demeaned = 1 - 1.675 = -0.675
#   Do this for all values in the original list.
#
# 4.) Return the demeaned values

# Hint: See the test cases below for how the calculation should work.
# If you're using NumPy or Pandas, you should change the None's to np.NaN's in
# the tests below.

def endTimer(start,msg):
    end = startTimer()
    diff = end - start
    l.info(msg.format(diff.total_seconds()))


def startTimer():
    start = datetime.now()
    return start

def group_adjust_panda(vals, groups, weights):
    l.info('group_adjust_panda')

    start = startTimer()

    # Creating data dictionary of lists vals + groups
    data = {'vals': vals}
    for i in range(len(groups)):
        #To provide column name for each group in groups
        data['Group ' + str(i)] = groups[i]
    endTimer(start, 'Total time to load dict: {}')
    # Creating a data frame from data dictionary
    df = pd.DataFrame(data)

    endTimer(start,'Total time to load df: {}')

    start = startTimer()
    # Generate weighted mean of by grouping val by each group items
    for i in range(len(groups)):
        df['Group ' + str(i)] = -1* df.groupby(['Group ' + str(i)])['vals'].transform('mean')*weights[i]
    if l.isEnabledFor(log.DEBUG):
        l.debug('df with weighted mean: \n' + str(df))
    endTimer(start, 'Total time to generate weighted mean df: {}')

    start = startTimer()
    #Transpose the data frame so that records across groups and val are a column
    df = df.transpose()
    endTimer(start, 'Total time to transpose df: {}')

    start = startTimer()
    #Sum of -ve weighted mean & vals would provide a row in the end with demeaned value
    #skipna=False will result in columns with NaN as val will result in NaN
    df=df.append(df.sum(skipna=False), ignore_index=True)
    endTimer(start, 'Total time to sum df: {}')

    #Demeaned list
    demeaned_list = df.loc[[len(groups) + 1]].values.tolist()[0]

    if l.isEnabledFor(log.DEBUG):
        l.debug('df with demean row: \n' + str(df))
        l.debug('demean list' + str(demeaned_list))

    return demeaned_list

def group_adjust(vals, groups, weights):
    """
    Calculate a group adjustment (demean).

    Parameters
    ----------

    vals    : List of floats/ints

        The original values to adjust

    groups  : List of Lists

        A list of groups.

    weights : List of floats

        A list of weights for the groupings.

    Returns
    -------

    A list-like demeaned version of the input values
    """

    if (len(weights)!=len(groups)):
        l.error('Length of weights and groups should be same')
        raise ValueError('Length of weights and groups should be same')

    for i in range(len(groups)):
        if (len(groups[i])!=len(vals)):
            l.error('Length of Group and Vals should be same')
            raise ValueError('Length of Group and Vals should be same')

    if isPanda:
        return group_adjust_panda(vals, groups, weights)
    else:
        return group_adjust_python(groups, vals, weights)


def group_adjust_python(groups, vals, weights):
    l.info('group_adjust_python')

    mean = {}
    for group in groups:
        #generate mean for each uniq value in group
        process(mean, group, vals)

    adj_vals = []
    records = len(vals)
    numGrps = len(groups)
    for i in range(records):
        adj_vals.append(0)
        for j in range(numGrps):
            #Sum weighted mean across group
            adj_vals[i] = adj_vals[i] + mean[groups[j][i]] * weights[j]
            #if l.isEnabledFor(log.DEBUG):
                #l.debug(str(adj_vals[i]) + '+' + str(mean[groups[j][i]]) + '(' + str(groups[j][i]) + ')' + '*' + str(weights[j]))
        #generate demeaned if val is not None else None
        adj_vals[i] = None if vals[i] is None else vals[i] - adj_vals[i]
    if l.isEnabledFor(log.DEBUG):
        l.debug('demean list' + str(adj_vals))
    return adj_vals


def process(mean, group,vals):
    items = list(set(group))
    records = len(vals)
    for item in items:
        total=0.0
        j=0
        for i in range(records):
            if group[i]==item and vals[i] is not None:
                total = vals[i] + total
                j=j+1

        mean[item] = total / j
    return mean

    pass
   # print mean





class GroupAdjustTest(unittest.TestCase):
    def test_three_groups(self):
        l.info('test_three_groups')
        vals = [1, 2, 3, 8, 5]
        grps_1 = ['USA', 'USA', 'USA', 'USA', 'USA']
        grps_2 = ['MA', 'MA', 'MA', 'RI', 'RI']
        grps_3 = ['WEYMOUTH', 'BOSTON', 'BOSTON', 'PROVIDENCE', 'PROVIDENCE']
        weights = [.15, .35, .5]

        adj_vals = group_adjust(vals, [grps_1, grps_2, grps_3], weights)
        # 1 - (USA_mean*.15 + MA_mean * .35 + WEYMOUTH_mean * .5)
        # 2 - (USA_mean*.15 + MA_mean * .35 + BOSTON_mean * .5)
        # 3 - (USA_mean*.15 + MA_mean * .35 + BOSTON_mean * .5)
        # etc ...
        # Plug in the numbers ...
        # 1 - (.15*2 + .35*2 + .5*1)   # -0.5
        # 2 - (.15*2 + .35*2 + .5*2.5) # -.25
        # 3 - (.15*2 + .35*2GroupAdjustTest + .5*2.5) # 0.75
        # etc...
        self.assertAlmostEqual(adj_vals[0], -0.77)
        self.assertAlmostEqual(adj_vals[1], -0.52)
        self.assertAlmostEqual(adj_vals[2], 0.480)
        self.assertAlmostEqual(adj_vals[3], 1.905)
        self.assertAlmostEqual(adj_vals[4], -1.095)



    def test_two_groups(self):
        l.info('test_two_groups')
        vals = [1, 2, 3, 8, 5]
        grps_1 = ['USA', 'USA', 'USA', 'USA', 'USA']
        grps_2 = ['MA', 'RI', 'CT', 'CT', 'CT']
        weights = [.65, .35]

        adj_vals = group_adjust(vals, [grps_1, grps_2], weights)
        # 1 - (.65*2 + .35*1)   # -0.65
        # 2 - (.65*2 + .35*2.5) # -.175
        # 3 - (.65*2 + .35*2.5) # -.825
        self.assertAlmostEqual(adj_vals[0], -1.819999999)
        self.assertAlmostEqual(adj_vals[1], -1.169999999)
        self.assertAlmostEqual(adj_vals[2], -1.336666666)
        self.assertAlmostEqual(adj_vals[3], 3.663333333)
        self.assertAlmostEqual(adj_vals[4], 0.6633333333)

    def test_missing_vals(self):
        l.info('test_missing_vals')
        # If you're using NumPy or Pandas, use np.NaN
        # If you're writing pyton, use None
        if isPanda:
            vals = [1, np.NaN, 3, 5, 8, 7]
        else:
            vals = [1, None, 3, 5, 8, 7]
        grps_1 = ['USA', 'USA', 'USA', 'USA', 'USA', 'USA']
        grps_2 = ['MA', 'RI', 'RI', 'CT', 'CT', 'CT']
        weights = [.65, .35]

        adj_vals = group_adjust(vals, [grps_1, grps_2], weights)
        #print adj_vals
        self.assertAlmostEqual(adj_vals[0], -2.47)
        # This should be None or np.NaN depending on your implementation
        # please feel free to change this line to match yours ]
        if (isPanda):
            self.assertTrue(np.isnan(adj_vals[1]))
        else:
            self.assertTrue(adj_vals[1] is None)
        self.assertAlmostEqual(adj_vals[2], -1.170)
        self.assertAlmostEqual(adj_vals[3], -0.4533333)
        self.assertAlmostEqual(adj_vals[4], 2.54666666)
        self.assertAlmostEqual(adj_vals[5], 1.54666666)

    def test_weights_len_equals_group_len(self):
        l.info('test_weights_len_equals_group_len')
        # Need to have 1 weight for each group
        vals = [1, 2, 3, 8, 5]
        grps_1 = ['USA', 'USA', 'USA', 'USA', 'USA']
        grps_2 = ['MA', 'MA', 'MA', 'RI', 'RI']
        grps_3 = ['WEYMOUTH', 'BOSTON', 'BOSTON', 'PROVIDENCE', 'PROVIDENCE']
        weights = [.15, .35]
        with self.assertRaises(ValueError) as context:
            adj_vals = group_adjust(vals, [grps_1, grps_2, grps_3], weights)
        pass

    def test_group_len_equals_vals_len(self):
        l.info('test_group_len_equals_vals_len')
        # The groups need to be same shape as vals
        vals = [1, 2, 3, 8, 5]
        grps_1 = ['USA', 'USA', 'USA', 'USA']
        grps_2 = ['MA', 'MA', 'MA', 'RI', 'RI']
        grps_3 = ['WEYMOUTH', 'BOSTON', 'BOSTON', 'PROVIDENCE', 'PROVIDENCE']
        weights = [.15, .35, .4]
        with self.assertRaises(ValueError) as context:
            adj_vals = group_adjust(vals, [grps_1, grps_2, grps_3], weights)
        pass

    def test_performance(self):
        l.info('test_performance')

        vals = 1000000 * [1, None, 3, 5, 8, 7]
        # If you're doing numpy, use the np.NaN instead
        # vals = 1000000*[1, np.NaN, 3, 5, 8, 7]
        grps_1 = 1000000 * [1, 1, 1, 1, 1, 1]
        grps_2 = 1000000 * [1, 1, 1, 1, 2, 2]
        grps_3 = 1000000 * [1, 2, 2, 3, 4, 5]
        weights = [.20, .30, .50]

        start = datetime.now()
        group_adjust(vals, [grps_1, grps_2, grps_3], weights)
        end = datetime.now()
        diff = end - start
        print 'Total performance test time: {}'.format(diff.total_seconds())

if __name__ == '__main__':
    FORMAT = '%(asctime)-15s : %(levelname)s : %(name)s : %(message)s'
    log.basicConfig(format=FORMAT)

    l=log.getLogger('group_adjust')
    l.setLevel(log.ERROR)

    isPanda = True
    unittest.main()


