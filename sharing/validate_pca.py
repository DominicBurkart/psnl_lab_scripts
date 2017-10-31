'''
PCA Validation

usage
    - to split the mturk data into two sets (test and training):
    python3 /path/to/this/file cleave /path/to/mturk/folder
    - NOTE: files used to validate the PCA will be output to the current directory.

    - to validate the PCA:
    python3 /path/to/this/file validate /path/to/split/folder/with/loadings
    - NOTE: in this folder, the PCA loadings file should be "loadings_from_spss.csv"
    - this folder should also be the directory from which you called the split version of this file.


Outline:    

1.) cleave the data according to the training_ratio, save the cleaves

2.) output the mean values for the 80 % to be PCA'd in SPSS

 ~~~~~ Get varimaxed PCA values and variable loadings from SPSS ~~~~~

3.) predict actual variables in 20% using loadings from the 80 % PCA

4.) record PCA predictive ability
'''

import os
import pickle


def cleave(directory):
    import pandas as pd

    training_ratio = 0.5

    def study_import(fname):  # for the sharing study
        import pandas as pd
        df = pd.read_csv(fname, skiprows=lambda x: x in [1, 2])  # standard for qualtrics
        unused = ['StartDate', 'EndDate', 'Status', 'IPAddress', 'Progress',
                  'Duration (in seconds)', 'Finished', 'RecordedDate',
                  'RecipientLastName', 'RecipientFirstName', 'RecipientEmail',
                  'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'Q26', 'Q31']
        l = df.columns.tolist()
        unused.extend([c for c in l if c.endswith("Click") or c.endswith("Submit") or c.endswith("Count")])
        return cleanup(df.drop(unused, axis=1), fname)

    def cleanup(df, fname):
        s = r"Sharing dims - MTurk - "
        e = r"  (divided"
        resp_ids = df['ResponseId']
        df = df.drop(['ResponseId'], axis=1)
        dimension = fname[fname.find(s) + len(s):fname.find(e)]
        l = df.columns.tolist()
        coldict = {}
        for c in l:
            coldict[c] = dimension + "__" + c
        df = df.rename(index=str, columns=coldict)
        return sessions_too([df[c].dropna() for c in df.columns.tolist()], resp_ids)

    def sessions_too(non_nas, resp_ids):
        return non_nas, [resp_ids.iloc[ser.index] for ser in non_nas]

    def o(obj, fname):
        n_bytes = 2 ** 31
        max_bytes = 2 ** 31 - 1
        bytes_out = pickle.dumps(obj)
        with open(fname, 'wb') as f_out:
            for idx in range(0, n_bytes, max_bytes):
                f_out.write(bytes_out[idx:idx + max_bytes])

    def split_by_participants_and_case(all_sessions, all_series, cleave=training_ratio):
        import itertools
        '''goal: maximize number of participants (sessions) in both of two groups (training and test) that can
        be used in a full random effects model for cross validation.

        implementation: permutively search for the case and participant split that satisfies the goal.

        '''

        def evaluate(c, p):
            '''evaluation: returns average and min # participants for each set.
            '''
            x0 = []
            x1 = []
            i = 0
            for case in c[0]:
                x0.append(0)
                x1.append(0)
                for counted in case.index:
                    if int(counted) in p:  # participant is in training set
                        x0[i] += 1
                    else:  # participant is in test set
                        x1[i] += 1
                i += 1
            return [[sum(x0) / len(x0), min(x0)], [sum(x1) / len(x1), min(x1)]]

        sessions = list(set([s for l in all_sessions for s in l.tolist()]))  # unique participants, arbitrary order
        sessions = [all_sessions[0][all_sessions[0] == s].index[0] for s in sessions]  # indices instead of session IDs
        sessions = [int(s) for s in sessions]

        questions = [s for s in all_series if s.dtype == 'float64']

        two_best = {'stats': [None, None], 'groups': [None, None]}  # two choices: overall max and max smallest set.

        for pp in itertools.permutations(sessions):
            for cp in itertools.permutations(questions):
                v = evaluate([cp[:int(cleave * len(cp))], cp[int(cleave * len(cp)):]], pp[:int(cleave * len(pp))])
                try:
                    if sum(sum(l) for l in v) > sum(sum(l) for l in two_best['stats'][0]):
                        two_best['stats'][0] = v
                        two_best['groups'][0] = [cp[:int(cleave * len(cp))], cp[int(cleave * len(cp)):]], [
                            pp[:int(cleave * len(pp))],
                            pp[int(cleave * len(pp)):]]
                        print("updated best choices. Stats (average and min group #s): " + str(two_best['stats']))
                except TypeError:
                    two_best['stats'][0] = v
                    two_best['groups'][0] = [cp[:int(cleave * len(cp))], cp[int(cleave * len(cp)):]], [
                        pp[:int(cleave * len(pp))],
                        pp[int(cleave * len(pp)):]]
                try:
                    if min(min(l) for l in v) > min(min(l) for l in two_best['stats'][1]):
                        two_best['stats'][1] = v
                        two_best['groups'][1] = [cp[:int(cleave * len(cp))], cp[int(cleave * len(cp)):]], [
                            pp[:int(cleave * len(pp))],
                            pp[int(cleave * len(pp)):]]
                        print("updated best choices. Stats (average and min group #s): " + str(two_best['stats']))
                except TypeError:
                    two_best['stats'][1] = v
                    two_best['groups'][1] = [cp[:int(cleave * len(cp))], cp[int(cleave * len(cp)):]], [
                        pp[:int(cleave * len(pp))],
                        pp[int(cleave * len(pp)):]]
        return two_best

    # code for pulling the questions from the qualtrics
    ob = "Sharing dims - MTurk - objective  (divided by 25)_August 1, 2017_13.47.csv"
    questions = pd.read_csv(os.path.join(directory, ob),
                            skiprows=range(2, pd.read_csv(os.path.join(directory, ob)).shape[0] + 1))

    test_set = []
    training_set = []
    means = {}
    devs = {}
    for fname in os.listdir(directory):
        if fname.endswith(".csv"):
            fname = os.path.join(directory, fname)
            all_series, all_sessions = study_import(fname)
            assert len(all_series) == len(all_sessions)

            two_best = split_by_participants_and_case(all_sessions, all_series)
            pd.DataFrame(two_best).to_csv("two_best.csv")
            sessions_set, series_set = two_best['groups'][0]

            for i in range(len(all_series)):
                series = all_series[i]
                sessions = all_sessions[i]
                assert series.name not in means.keys()
                if len(series) == 0:
                    print("excluding " + series.name + " as it contains no values.")
                else:
                    try:
                        # populate means and devs, get z scores
                        means[series.name] = series.mean()
                        devs[series.name] = series.std()
                        old = series
                        series = (series - means[series]) / devs[series]

                        # untested
                        training_set.append([series.iloc[si] for si in series.index if sessions_set[si] == 0])
                        test_set.append([series.iloc[si] for si in series.index if sessions_set[si] == 1])
                        print("training set: " + str(training_set))
                        print("testing set: " + str(testing_set))
                        # /untested
                    except (TypeError, ValueError):
                        print("excluding " + series.name + " as it is not numerically typed.")

    print("min # responses in test set: " + str(min([len(l) for l in test_set])))
    print("min # responses in training set: " + str(min([len(l) for l in training_set])))

    o([training_set, test_set], "sets.pickle")  # save test and training sets for later.

    keys = list(means.keys())

    Qs = list(set([x.split("__")[1] for x in keys]))  # questions (cases)
    Ds = list(set([x.split("__")[0] for x in keys]))  # dimensions (variables)

    temp = []
    tempq = []

    for q in Qs:
        row = {}
        for d in Ds:
            for k in keys:
                if k.startswith(d) and k.endswith(q):
                    row[d] = means[k]
                    break
        temp.append(row)
        tempq.append(q)

    all_keys = set(temp[0].keys())
    i = 0
    bads = []
    clean = []
    for row in temp:
        if set(row.keys()) != all_keys:
            bads.append(i)
            del tempq[i]
        else:
            clean.append(row)
        i = i + 1

    assert len(tempq) == len(clean)

    df = pd.DataFrame(columns=clean[0].keys())

    for i in range(len(clean)):
        try:
            df.loc[questions[tempq[i]][0].split(r" - ")[1]] = clean[i]
        except IndexError:
            df.loc[questions[tempq[i]][0]] = clean[i]
        except KeyError:
            print(tempq[i] + " excluded from the analysis as it is not rated along all dimensions.")
            continue

    df.to_csv("scored_questions_for_r.csv")


# ~~~~~ Get loadings from R script ~~~~~


# # 3.) predict actual variables in 20% using loadings from the 80 % PCA
# # 4.) record PCA predictive ability
# def test_predictiveness(directory):
#     import numpy
#
#     def l(file_path):
#         n_bytes = 2 ** 31
#         max_bytes = 2 ** 31 - 1
#         bytes_in = bytearray(0)
#         input_size = os.path.getsize(file_path)
#         with open(file_path, 'rb') as f_in:
#             for _ in range(0, input_size, max_bytes):
#                 bytes_in += f_in.read(max_bytes)
#         return pickle.loads(bytes_in)
#
#     loadings = numpy.genfromtxt("loadings_from_spss.csv", delimiter=',')
#     eigenvalues = numpy.genfromtxt("eigenvalues_from_spss.csv", delimiter=",")
#     ##    components = numpy.genfromtxt("components_from_spss.csv", delimiter=',')
#     training_set, test_set = l(os.path.join(directory, "sets.pickle"))
#
#     assert len(training_set) == len(test_set)
#     # we have the same number of dimensions in the test and training set.
#
#     assert all([len(training_set[i]) > len(test_set[i]) for i in len(test_set)])
#     # the test set has fewer observations than the training set.
#
#     assert loadings.shape[0] == len(test_set)
#
#     # we have loadings for the same number of variables as we do test/training data.
#
#     def reconstruct_X(scores, loadings):
#         '''predicts original variables based on component values and loadings.
#         formulae from Abdi, H., & Williams, L. J. (2010). Principal component analysis.
#         Wiley interdisciplinary reviews: computational statistics, 2(4), 433-459.
#         '''
#         Qt = numpy.transpose(loadings)
#         return numpy.dot(scores, Qt)
#
#     def get_factors(variables, loadings):
#         '''calculates component values given variables, based on established loadings.'''
#         return numpy.dot(variables, loadings)
#
#     def squared_differences(variables, loadings):  # sum of matrix this returns == RESS
#         return numpy.sum(
#             numpy.square(numpy.subtract(variables, reconstruct_X(get_factors(variables, loadings), loadings))))
#
#     total_inertia = numpy.sum(numpy.square(training_set))
#
#     training_sq = squared_differences(training_set, loadings)
#
#     assert total_inertia - sum(eigenvalues) == math.sqrt(numpy.sum(training_sq))  # these should be identical.
#
#     testing_sq = squared_differences(training_set, loadings)
#
#     return training_sq, testing_sq
#
#     # press_testing should be approximately equal to ress_training. How do we quantify that with only two datapoints?
#     # maybe by performing a KS test or similar on the squared difference between the actual and estimated values?
#     # ask mark.
#
#     print(ks_2samp(training_sq, testing_sq))


if __name__ == '__main__':
    import sys

    if sys.argv[1] == "cleave":
        cleave(sys.argv[2])
    elif sys.argv[1] == "validate":
        print("under construction")
        #test_predictiveness(sys.argv[2])
