---
title: 'kerasR: R Interface to the Keras Deep Learning Library'
tags:
  - neural networks
  - convolutional neural networks
  - recurrent neural networks
  - computer vision
  - natural language processing
authors:
 - name: Taylor B Arnold
   orcid: 0000-0003-0576-0669
   affiliation: 1
affiliations:
 - name: University of Richmond, Department of Mathematics and Computer Science
   index: 1
date: 01 June 2017
bibliography: paper.bib
---

# Summary

Keras is a high-level neural networks API, originall written in Python, and
capable of running on top of either TensorFlow or Theano. It was developed
with a focus on enabling fast experimentation. This package provides an
interface to Keras from within R. All of the returned objects from functions
in this package are either native R objects or raw pointers to python
objects, making it possible for users to access the entire keras API.
The main benefits of the package are (1) correct, manual parsing of R
inputs to python, (2) R-sided documentation, and (3) examples written
using the API. It allows, amongst other things, users to load and run
popular pre-trained models such as VGG-19 [@he2015delving],
ResNet50 [@he2016deep], and Inception [@szegedy2015going].

Most functions have associated examples showing a working example of how
a layer or object may be used. These are mostly toy examples, made with
small datasets with little regard to whether these are the correct models
for a particular task. See the package vignettes for a more thorough
explaination and several larger, more practical examples.

# References
