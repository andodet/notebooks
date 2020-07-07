"""Helper functions for feature engineering"""

import numpy as np
from collections import Counter
import re
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import _VectorizerMixin
from sklearn.feature_selection._base import SelectorMixin

# Transaction features
def total_amt(row):
    '''Return total transacted amount'''
    return np.sum([tx['transactionAmount'] for tx in row])

def avg_amt(row):
    '''Return average transacted amount'''
    return np.mean([tx['transactionAmount'] for tx in row])

def n_tx_failed(row):
    '''Returns the number of failed transactions'''
    failed_txs = 0
    for tx in row:
        if tx['transactionFailed'] == True:
            failed_txs += 1
    return failed_txs

# Payment methods features
def pm_registration_fails(row):
    '''Return number of failed payment registration failed attempts'''
    failed_regs = 0
    for pm in row:
        if pm['paymentMethodRegistrationFailure'] == True:
            failed_regs += 1
    return failed_regs

def n_distinct(row, key):
    '''Returns number of distinct values for a given key '''
    c = Counter()
    for pm in row:
        for k, v in pm.items():
            if k == key:
                c.update([v])
    return len(c)

def get_payment_methods(row):
    '''Return number of failed payment registration failed attempts'''
    pm_counter = Counter()
    for i in row:
        for k, v in i.items():
            if k == 'paymentMethodType':
                pm_counter.update([v])
    return dict(pm_counter)

# Orders features
def tot_amt_ordered(row):
    '''Returns total amount (monetary) ordered'''
    return np.sum([txn['orderAmount'] for txn in row])

def avg_ordered(row):
    '''Returns average amount (monetary) ordered'''
    return np.mean([txn['orderAmount'] for txn in row])

def order_states(row):
    '''Returns order state counts'''
    states_count = Counter()
    for o in row:
        for k, v in o.items():
            if k == 'orderState':
                states_count.update([v])
    return dict(states_count)

def get_n_addresses(row):
    '''Returns number of distinct shipping addresses used'''
    addr_counter = Counter()
    for order in row:
        for k, v in order.items():
            if k == 'orderShippingAddress':
                addr_counter.update([v])
    return dict(addr_counter)

def get_addr_states(row):
    '''Extract zip code from shipping address'''
    states = Counter()
    for o in row:
        for k, v in o.items():
            if k == 'orderShippingAddress':
                res = re.search(r"[A-Z]{2}", v)
                states.update([res.group(0)])
    return dict(states)

def get_zipcode(row):
    '''Extract zip code from shipping address'''
    zip_codes = Counter()
    for o in row:
        for k, v in o.items():
            if k == 'orderShippingAddress':
                res = re.search(r"(\d+)$", v)
                zip_codes.update([res.group(0)])
    return dict(zip_codes)

# Customers
def vowels_ratio(row):
    vowels = ["a", "e", "i", "o", "u"]
    email = row['customerEmail']
    username = email.split("@")[0]
    vowels_count = 0
    for char in username:
        if char in vowels:
            vowels_count += 1.0  # Need a float for division
    return vowels_count / len(email)

def get_email_domain(row):
    email = row['customerEmail']
    return email.split("@")[1]

def email_length(row):
    '''Returns how long is enmail username'''
    email = row['customerEmail']
    return len(email)

def get_phone_prefix(row):
    '''Returns phoen prefix'''
    print(row['customerPhone'])
    prefix = re.search(r'^(\d+)', row['customerPhone'])
    return prefix.group(0)
    
def address_mismatch(row):
    '''Checks if billing address appears in the shipping addresses'''
    billing_addr = row['customer']['customerBillingAddress']
    shipping_addr = []
    for order in row['orders']:
        shipping_addr.append(order['orderShippingAddress'])
    if billing_addr not in shipping_addr:
        return True
    else:
        return False
    
    
# sklearn has no obvious way to retain feature names after column transformer
# has been applied (https://stackoverflow.com/a/57534118/9046275)
def get_feature_out(estimator, feature_in):
    if hasattr(estimator,'get_feature_names'):
        if isinstance(estimator, _VectorizerMixin):
            # handling all vectorizers
            return [f'vec_{f}' \
                for f in estimator.get_feature_names()]
        else:
            return estimator.get_feature_names()
    elif isinstance(estimator, SelectorMixin):
        return np.array(feature_in)[estimator.get_support()]
    else:
        return feature_in


def get_ct_feature_names(ct):
    output_features = []

    for name, estimator, features in ct.transformers_:
        if name!='remainder':
            if isinstance(estimator, Pipeline):
                current_features = features
                for step in estimator:
                    current_features = get_feature_out(step, current_features)
                features_out = current_features
            else:
                features_out = get_feature_out(estimator, features)
            output_features.extend(features_out)
        elif estimator=='passthrough':
            output_features.extend(ct._feature_names_in[features])
                
    return output_features