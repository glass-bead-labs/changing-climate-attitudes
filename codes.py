'''codes.py - data and classes for manipulating our mechanism codes'''

import re
from itertools import chain
from warnings import warn

import numpy as np
import pandas as pd

# Makes the module pretty specific to our use-case
sep_codes = ['mccs1', 'mccs2', 'mccs3', 'mccs4',
             'dnk1', 'dnk2',
             'mccq1', 'mccq2',
             'sghg3', 'mccr', 'dd3',
             'dghg3', 'dghg4',
             'mcce1', 'mcce2', 'mcce3', 'mcce4']
cluster_codes = [['sghg1', 'sghg2'], ['dd1', 'dd2'], ['dghg1', 'dghg2']]
all_codes = sorted(chain(sep_codes, *cluster_codes))

code_categories = {'ghg':    [['sghg1', 'sghg2', 'sghg3', 'mccs3', 'mccr'],
                              ['dghg2'],
                              ['dghg1'] ],
                   'light':  [['dd2'],
                              ['dd1'] ],
                   'energy': [['mcce4'],
                              ['mcce3','mcce2'],
                              ['mcce1'] ] }

def compute_score_vector(avg_score_mat):
    '''Compute total score for each row of AvgMatrix

    avg_score_mat : DataFrame
        As comes from AvgMatrix.df below
    '''
    null_rows = []
    for lab, r in avg_score_mat.iterrows():
        if r.isnull().any():
            null_rows.append(lab)

    subscores = pd.DataFrame(0.0, index=avg_score_mat.index,
                             columns=code_categories.keys() )

    for category, code_levels in code_categories.iteritems():
        for level, codes in enumerate(code_levels, 1):
            # Check for any votes (i.e., non-zero score) in current set of codes
            found = avg_score_mat[codes].any(axis=1)
            # Update to the current score level where any codes were found
            subscores[category][found] = level

    subscores['light'] *= 3.0/2

    subscores.loc[null_rows] = np.nan

    return subscores


class AvgMatrix:
    """Matrix representing codes that can be used easily for computation"""

    def __init__(self, codes_df):
        """Convert "sparse" coding format used by Reasoners to "full"

        codes_df : DataFrame
            One column per coder. Index should represent participant /
            survey number.
        # extra_index : Index (optional)
        #    Will be added to codes_df.index to create MultiIndex
        """
        # We use a DataFrame to allow us to index the columns by name, and the
        # rows according to the original (probably integer) labels
        self.df = pd.DataFrame(np.zeros((len(codes_df), len(all_codes))),
                    index=codes_df.index,
                    # index=[core_df.index, [timedelta(-1)] * len(core_df)],
                    columns=all_codes)

        self.build_code_matrix(codes_df)
        too_big = self.df.max() > 1
        if any(too_big):
            warn('>1 value(s) in: %s' % ', '.join(self.df.columns[too_big]) )

    def build_code_matrix(self, codes_df):
        '''Increment number of times each text is given a certain code

        codes_df : DataFrame
            A column for each rater, codes are separated by commas and maybe
            spaces
        '''
        for lab, code_row in codes_df.iterrows():
            has_codes = code_row.notnull()
            if any(has_codes):
                # Average only columns with codes
                for d in code_row[has_codes]:
                    self.incr_codes(lab, d, 1.0 / sum(has_codes))
            else:
                # Here, broadcasting nan's to the whole row
                self.df.loc[lab] = np.nan

    def incr_codes(self, row, codes, val):
        # Split on any combo of ?, comma, or space
        # We use filter to throw out ''
        # set gets rid of duplicates
        split_codes = set(filter(None, re.split('[?, ]+', codes.lower())))
        for c in split_codes:
            self.df.at[row, c] += val

    @property
    def scores(self):
        '''Scores broken down by category'''
        try:
            return self._scores
        except AttributeError:
            self._scores = compute_score_vector(self.df)
            return self._scores

    @property
    def total_scores(self):
        '''Total sum of all scores per participant'''
        try:
            return self._totals
        except AttributeError:
            # I left this on a separate line to catch errors separately
            scores = self.scores
            self._totals = scores.sum(axis=1)
            return self._totals
