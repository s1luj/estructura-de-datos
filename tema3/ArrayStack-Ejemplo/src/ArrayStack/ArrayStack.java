package ArrayStack;

import java.util.Arrays;

public class ArrayStack<T> implements Stack<T> {
	private T[] elements;
	private int nextFree;
	
	public ArrayStack() {
		elements=(T[]) new Object[10];
		nextFree=0;
	}
	
	public boolean isEmpty() {
		if (nextFree == 0) {
			return true;
		} else {
			return false;
		}
	}
	
	public void ensureCapacity() {
		if (nextFree==elements.length) {
			elements = Arrays.copyOf(elements, elements.length*2);
		}
	}
	
	public void push(T elem) {
		ensureCapacity();
		elements[nextFree]=elem;
		nextFree++;
	}
	
	public T top() throws ArrayStackException {
		if (isEmpty()) {
			throw new ArrayStackException("La pila está vacia");
		} else {
			return elements[nextFree-1];
		}
	}
	
	public void pop() {
		if (isEmpty()) {
			throw new ArrayStackException("La pila está vacia");
		} else {
			nextFree--;
		}
	}
	
	public String toString() {
		StringBuilder s= null;
		s.append("[");
		for (int i=0; i<nextFree; i++){
			s.append(elements[i]+", ");
		}
		s.append("]");
		return s.toString();
	}
	
	
	
}
