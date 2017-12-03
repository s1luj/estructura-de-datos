/********************************************************************
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 4.
 * Ejercicio 12.b de la tercera relación. Implementar el TAD Bolsa
 *
 * Alumno: FERNÁNDEZ MÁRQUEZ, JOSÉ LUIS
 ********************************************************************/

package dataStructures.bag;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class SortedLinkedBag<T extends Comparable<? super T>> implements Bag<T> {

	static private class Node<E> {
		E elem;
		int count;
		Node<E> next;

		Node(E x, int n, Node<E> node) {
			elem = x;
			count = n;
			next = node;
		}
	}

	/**
	 * Invariant:
	 *
	 * 1. keep the linked list sorted by "elem", with no duplicates
	 *
	 * 2. the counter "count" must be positive
	 *
	 * Example: first -> ('a', 5) -> ('d', 1) -> ('t', 3) -> ('z', 2)
	 *
	 */
	private Node<T> first;

	public SortedLinkedBag() {
		Node<T> first = null;
	}

	@Override
	public boolean isEmpty() {
		return first==null;
	}

	@Override
	public void insert(T item) {
		Node<T> previous = null;
		Node<T> current = first;

		while (current != null && current.elem.compareTo(item) < 0) {
			previous = current;
			current = current.next;
		}

		if (current != null && current.elem.equals(item)) { //hemos encontrado el elemento
		   //aumentamos count
		   current.count++;
		   // TO BE FILLED OUT

		  } else if (previous == null) { //no hay elementos
		   first = new Node<T>(item, 1, first);
		  } else {// no existe, hay que insertarlo

		   // TO BE FILLED OUT
		   Node<T> aux = new Node<T>(item, 1, current);
		   previous.next = aux;

		  }

	}
		// TODO Auto-generated method stub
/*		Node<T> previous = null;
		Node<T> current = first;
		while (current!=null && current.elem.compareTo(item)<0) {
			previous = current;
			current = current.next;
		}
		if (current.elem.equals(item)) {
			current.count++;
		} else if (current!=null) {
			Node<T> nuevo = new Node<T>(item,1,current);
			previous.next=nuevo;
		} else {
			current = new Node<T>(item, 1, null);
			// first = new Node<T>(item,1,first);
 		}
		
	}	
*/		

	@Override
	public int occurrences(T item) {
		// TODO Auto-generated method stub
		Node<T> current = first;
		while (current != null && current.elem.compareTo(item)<0 ) {
			current = current.next;
		}
		if (current != null && current.elem.equals(item)){
			return current.count;
		} else {
			return 0;
		}
	}

	@Override
	public void delete(T item) {
		Node<T> previous = null;
		Node<T> current = first;

		while (current != null && current.elem.compareTo(item) < 0) {
		   previous = current;
		   current = current.next;
		}

		if (current != null && current.elem.equals(item)) {//lo hemos encontrado
			if (current.count > 1) {// tiene mas de uno
				current.count--;
			} else if (previous == null) {//es el primero 
				first = first.next;

			} else {
				previous.next = current.next;
			}
		}
		
/*		// TODO Auto-generated method stub
		Node<T> current=first;
		Node<T> previous=null;
		while (current.elem.compareTo(item)<0 && current!=null) {
			previous=current;
			current=current.next;
		}
		if (current.elem.equals(item)) {
			if (current.count>1) {
				current.count--;
			} else {
				previous.next=current.next;
			}
//		} else {
//			throw new RuntimeException("DELETE ERROR: el elemento a borrar no esta en la bolsa");
		}
*/	}

	@Override
	public void copyOf(Bag<T> source) {
		// TODO Auto-generated method stub
		Iterator<T> it = source.iterator();
		this.first = null;
		while (it.hasNext()) {
			T x = it.next();
			int n = source.occurrences(x);
			for (int i = 0 ; i<n; i++) {
				source.insert(x);
			}
		}
		
	}

	@Override
	public String toString() {
		StringBuilder output = new StringBuilder("LinkedBag(");
		for (Node<T> p = first; p != null; p = p.next) {
			output.append("(").append(p.elem).append(", ").append(p.count).append(") ");
		}
		return output.append(")").toString();
	}

	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return new SortedLinkedBagIterator();
	}
	private class SortedLinkedBagIterator implements Iterator<T>{
		Node<T> current;
		public SortedLinkedBagIterator(){
			current=first;
		}
		@Override
		public boolean hasNext() {
			// TODO Auto-generated method stub
			return current!=null;
		}

		@Override
		public T next() {
			// TODO Auto-generated method stub
			if (!hasNext()) {
				throw new NoSuchElementException();
			}
			T x = current.elem; // visitamos el nodo que toca
			current=current.next; // avanzamos al siguiente
			return x;
		}
		
	}
}
