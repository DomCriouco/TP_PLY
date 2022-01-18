# -*- coding: utf-8 -*-
'''
Author : Vincent Genin ESGI-3AL 2018
'''

import uuid
import graphviz as gv

def printTreeGraph(t):
    graph = gv.Digraph(format='pdf')
    graph.attr('node', shape='circle')
    addNode(graph, t)
    #graph.render(filename='img/graph') #Pour Sauvegarder
    graph.view() #Pour afficher

def addNode(graph, t):
    myId = uuid.uuid4()

    if type(t) != tuple:
        graph.node(str(myId), label=str(t))
        return myId

    graph.node(str(myId), label=str(t[0]))
    for i in range(1, len(t)):
         graph.edge(str(myId), str(addNode(graph, t[i])), arrowsize='0')


    return myId
    
#printTreeGraph(('+', 'lknlnk', ('*', 4, 6, 3)))

#printTreeGraph(('S', ('S', 'n'), 'a',  ('S', ('S', 'n'), 'b', ('S', 'n'))))

#tree=('tab vide', ('tab[0]=0', ('tab[1]=0',' tab[2]=0 \n\n 000', 'tab[2]=1\n\n 001'), ('tab[1]=1',' tab[2]=0\n\n 010', 'tab[2]=1\n\n 011')), ('tab[0]=1',('tab[1]=0',' tab[2]=0\n\n 100', 'tab[2]=1\n\n 101'), ('tab[1]=1',' tab[2]=0\n\n 110', 'tab[2]=1\n\n 111')))
