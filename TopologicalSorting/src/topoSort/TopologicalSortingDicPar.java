/**
 * Estructuras de Datos
 * Práctica 9 - Orden topológico en digrafos (sin clonar el grafo)
 *
 * APELLIDOS :   FERNÁNDEZ MÁRQUEZ         NOMBRE: JOSÉ LUIS
 */

package topoSort;

import dataStructures.dictionary.Dictionary;
import dataStructures.dictionary.HashDictionary;
import dataStructures.graph.DiGraph;
import dataStructures.list.ArrayList;
import dataStructures.list.List;
import dataStructures.set.Set;

public class TopologicalSortingDicPar<V> {

	private List<Set<V>> topSort;
	private boolean hasCycle;

	public TopologicalSortingDicPar(DiGraph<V> graph) {

		topSort = new ArrayList<>();
		// dictionary: vertex -> # of pending predecessors
		Dictionary<V, Integer> pendingPredecessors = new HashDictionary<>();
		Set<V> sources;

		// todo
	}

	public boolean hasCycle() {
		return hasCycle;
	}

	public List<Set<V>> order() {
		return hasCycle ? null : topSort;
	}

}
