/********************************************************************
 * Estructuras de Datos. 2º Curso. ETSI Informática. UMA
 * PRACTICA 4.
 * Ejercicio 12.b de la tercera relación. Implementar el TAD Bolsa
 *
 * Alumno: APELLIDOS, NOMBRE
 ********************************************************************/

package dataStructures.bag;

import java.util.Iterator;

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
		// TODO Auto-generated method stub
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void insert(T item) {
		// TODO Auto-generated method stub
	}

	@Override
	public int occurrences(T item) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void delete(T item) {
		// TODO Auto-generated method stub
	}

	@Override
	public void copyOf(Bag<T> source) {
		// TODO Auto-generated method stub
		// the implementation of 'copyOf' cannot use the iterator
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
		return null;
	}
}
