/**
 * Estructuras de Datos
 * Práctica 9 - Orden topológico en digrafos (sin clonar el grafo)
 *
 * APELLIDOS :  FERNÁNDEZ MÁRQUEZ        NOMBRE: JOSÉ LUIS
 */

package topoSort;

import java.util.Set;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.DiGraph;
import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.queue.LinkedQueue;
import dataStructures.queue.Queue;

public class TopologicalSortingDic<V> {

	private List<V> topSort;
	private boolean hasCycle;

	public TopologicalSortingDic(DiGraph<V> graph) {

		topSort = new ArrayList<>();
		// dictionary: vertex -> # of pending predecessors
		Dictionary<V, Integer> pendingPredecessors = new HashDictionary<>();
		Queue<V> sources = new LinkedQueue<>();

		// todo
		for (V v : graph.vertices()) {
			pendingPredecessors.insert(v, graph.inDegree(v));
		}
		hasCycle = false;
		while (pendingPredecessors.isEmpty()&& !(hasCycle)) {
			for(V k : pendingPredecessors.keys()) {
				if (pendingPredecessors.valueOf(k)==0) {
					sources.enqueue((V) pendingPredecessors.valueOf(k));
				}
			}
			
		}
		hasCycle = sources.isEmpty();
		
		while (!sources.isEmpty()){
			V source = sources.first();
			sources.dequeue();
			pendingPredecessors.delete(source);
			topSort.append(source);
			for (V v : graph.successors(source)) {
				pendingPredecessors.insert(v, pendingPredecessors.valueOf(v)-1);
			}

		}
	
	}

	public boolean hasCycle() {
		return hasCycle;
	}

	public List<V> order() {
		return hasCycle ? null : topSort;
	}
}
