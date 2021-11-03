#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 24 13:05:49 2020

@author: stevekimbrough
"""

import math 
import random



class Conversant():
    '''
    This agent is operating in a world in which uttering a 'vowel' is
    easiest the further it is away from the leftLimit and the rightLimit.
    Also, the vowel is best understood the closer it is to the idealPoint.
    The agent learns by Probe and Adjust.
    The agent is initialized with a value of mu, for which it probes and
    adjusts.
    '''

    
    def __init__(self,ID=0,leftLimit=0,rightLimit=100,idealPoint=50,
                 initialMu=50,epochLength=50,numUtterances=500,
                 theK=0.041,epsilon=0.5,idealFactor=1.0):
        self.ID = ID
        self.leftLimit=leftLimit
        self.rightLimit=rightLimit
        self.idealPoint=idealPoint
        self.initialMu=initialMu
        self.epochLength=epochLength
        self.mu = self.initialMu
        self.sayHigh = []
        self.sayLow = []
        self.epochCount = 0
        self.utteranceCount = 0
        self.numUtterances = numUtterances
        self.theK=theK
        self.epsilon=epsilon
        self.idealFactor = idealFactor
        
    def go(self):
        self.utteranceCount = 0
        while self.utteranceCount < self.numUtterances:
            self.epochCount = 0
            while self.epochCount < self.epochLength:
                if self.utteranceCount >= self.numUtterances:
                    break
                #print("length high = {}, length Low = {} epoch count = {} {}".format(len(self.sayHigh),len(self.sayLow),self.epochCount,self.utteranceCount))
                #self.epochCount += 1
                self.speak() #random.normalvariate(self.mu,1)
#                if utterance >= self.mu:
#                    self.sayHigh.append(utterance)
#                else:
#                    self.sayLow.append(utterance)
            

    def speak(self):
        '''
        The conversant speaks and returns a normal variate.
        And updates its epoch if appropriate.
        '''
        self.epochCount += 1
        self.utteranceCount += 1
        utterance = random.normalvariate(self.mu,1)
        if utterance >= self.mu:
            self.sayHigh.append(utterance)
        else:
            self.sayLow.append(utterance)
        #if len(self.sayHigh) + len(self.sayLow) == self.epochLength:
        if self.epochCount >= self.epochLength:
            self.updateEpoch()
        #print("sayHigh",self.sayHigh)
        #print("sayLow",self.utteranceCount,self.sayLow)
        return (utterance,self.mu)

    def nlogospeak(self):
        '''
        The conversant speaks and returns a normal variate.
        And updates its epoch if appropriate.
        '''
        self.epochCount += 1
        #self.utteranceCount += 1
        utterance = random.normalvariate(self.mu,1)
        if utterance >= self.mu:
            self.sayHigh.append(utterance)
        else:
            self.sayLow.append(utterance)
        #if len(self.sayHigh) + len(self.sayLow) == self.epochLength:
        if self.epochCount >= self.epochLength:
            self.nlogoupdateEpoch()
        #print("sayHigh",self.sayHigh)
        #print("sayLow",self.utteranceCount,self.sayLow)
        return (utterance,self.mu)

    def nlogoupdateEpoch(self):
        '''
        Set mu to the mean of whichever list of utterances has a
        better score.
        '''
# =============================================================================         
        #print("updating epoch. utteranceCount = {}".format(self.utteranceCount))
#         print("epoch count = {}".format(self.epochCount))
#         print(self.sayHigh)
#         print(self.sayLow)
#         print([self.getUtteranceScore(x,self.theK) for x in self.sayHigh])
#         print([self.getUtteranceScore(x,self.theK) for 
#                          x in self.sayLow])
# =============================================================================
        highScore = sum([self.getUtteranceScore(x,self.theK) for 
                         x in self.sayHigh])/len(self.sayHigh)
        lowScore = sum([self.getUtteranceScore(x,self.theK) for 
                         x in self.sayLow])/len(self.sayLow)
        if highScore >= lowScore:
            self.mu += self.epsilon
        else:
            self.mu -= self.epsilon
        self.sayHigh = []
        self.sayLow = []
        # Set it high to terminate the while loop
        self.epochCount = 0 #self.epochLength
    
    def updateEpoch(self):
        '''
        Set mu to the mean of whichever list of utterances has a
        better score.
        '''
# =============================================================================         
        #print("updating epoch. utteranceCount = {}".format(self.utteranceCount))
#         print("epoch count = {}".format(self.epochCount))
#         print(self.sayHigh)
#         print(self.sayLow)
#         print([self.getUtteranceScore(x,self.theK) for x in self.sayHigh])
#         print([self.getUtteranceScore(x,self.theK) for 
#                          x in self.sayLow])
# =============================================================================
        highScore = sum([self.getUtteranceScore(x,self.theK) for 
                         x in self.sayHigh])/len(self.sayHigh)
        lowScore = sum([self.getUtteranceScore(x,self.theK) for 
                         x in self.sayLow])/len(self.sayLow)
        if highScore >= lowScore:
            self.mu += self.epsilon
        else:
            self.mu -= self.epsilon
        self.sayHigh = []
        self.sayLow = []
        # Set it high to terminate the while loop
        self.epochCount = self.epochLength
            
        

    def mirroredPositiveLogistic(self,x,k):
        '''
        After the reporter in Model 2 simple selection.nlogo
        From Model 2, simple selection
                (1 - (1 / ((1 + e ^ (-1 *  the-k *  x))))) * 2
                report (1 - (1 / ((1 + e ^ (-1 *  the-k *  x))))) * 2
                
        x is the distance to 0
        '''
        return (1 - (1/((1+math.e**(-1*k*x)))))*2
                
    def getUtteranceScore(self,x,k):
        '''
        This is the score of a single utterance, a draw from a
        normal distribution. For now, just the simplest of
        scoring, adding up the three values
        '''
        # score for leftLimit
        lscore = 1 - self.mirroredPositiveLogistic(abs(x-self.leftLimit),k)
        score = lscore
        # score for rightLimit
        lscore = 1 - self.mirroredPositiveLogistic(abs(x-self.rightLimit),k)
        score += lscore 
        # score for idealPoint
        lscore = self.mirroredPositiveLogistic(abs(x-self.idealPoint),k)
        score += lscore * self.idealFactor
        return score

#global conversantList
conversantList = []
global carol
carol  = Conversant()

def nlogoinstance(left,right,ideal,idealf):
    global carol
    carol = Conversant(leftLimit=left,
                       rightLimit=right,idealPoint=ideal,idealFactor=idealf)
    
    
def nlogospeak():
    global carol
    (x,mu) = carol.nlogospeak()
    return [x,mu]
    
def makeConversants(n=3):
    for idx in range(n):
        conversantList.append(Conversant(ID=idx))    

def nlogospeakconversants():
    for idx in range(len(conversantList)):
        conversantList[idx].nlogospeak()
               

        
if __name__ == '__main__':
    pass
    bob = Conversant(idealPoint=80,numUtterances=3550,idealFactor=15)
    bob.go()
    print(bob.mu,bob.sayHigh,bob.sayLow)
# =============================================================================
#     nlogoinstance(0,100,74,1.0)
#     for inx in range(1811):
#         nlogospeak()
#     print(carol.mu)
#     x = makeConversants()
# =============================================================================
    makeConversants(5)
    nlogospeakconversants()
