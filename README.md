# Technological Proximity and Innovation Strategies

This repository contains the code and data for the project "Technological Proximity and Innovation Strategies," conducted by Claire Bresson, Thomas Chen, Elvis Gaba-Kpayedo, and Patryk Wisniewski as part of the Cycle Ingénieur 2A at ENSAE. The project is supervised by Antonin Bergeaud and Clément Malgouyres.

The project investigates the impact of strategic patenting on innovation networks, using data from the Google Patents Database. It employs a microeconomic model and difference-in-differences approach to analyze firm blocking behaviors and their effects on innovation levels and patent value.


## Data

The primary data source for this project is the Google Patents Database, accessible via Google Cloud and BigQuery. Additional data from the publication "Technological Innovation, Resource Allocation, and Growth" by Kogan et al. (2017) is also utilized.

## Building the Innovation Network

The innovation network is constructed using forward citations received within five years following a patent's publication date. The network is divided into clusters using the Leiden clustering method, which provides more stable and well-connected communities compared to the Louvain method.
Centrality Measures

Two centrality measures are used to assess the importance of companies within each cluster:

    Betweenness Centrality: Measures the importance of a company in terms of connectivity and communication within the network.
    Closeness Centrality: Measures the ability of a company to rapidly disseminate information throughout the network.

## Difference-in-Differences Framework

A difference-in-differences framework is employed to examine the effects of patent issuance on subsequent innovation, utilizing the variance in processing times of patents by the USPTO, particularly around holidays.
Results

## The analysis reveals significant insights into the impact of blocking behaviors and firm strategies on innovation networks. Key findings include:

    The effect of patent issuance on the number of subsequent citations.
    The role of centrality measures in predicting future patenting activity.
    The impact of strategic patenting on the larger innovation network.
