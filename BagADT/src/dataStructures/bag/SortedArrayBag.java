/********************************************************************
 * Estructuras de Datos. 2º ETSI Informática. UMA
 * PRACTICA 4.
 * Ejercicio 12.b de la tercera relación. Implementar el TAD Bolsa.
 *
 * Alumno: APELLIDOS, NOMBRE
 ********************************************************************/

package dataStructures.bag;

import java.util.Arrays;
import java.util.Iterator;

public class SortedArrayBag<T extends Comparable<? super T>> implements Bag<T> {

	private final static int INITIAL_CAPACITY = 1;

	// The bag is represented by two arrays ("value" and "count") and
	// a cursor ("nextFree").
	//
	// The bag is stored in the first "nextFree-1" positions of
	// arrays "value" and "count". Cursor "nextFree" refers to the
	// first position available in both arrays (if any).
	//
	// The values are stored sorted in the array "value", the counter
	// corresponding to "value[i]" is stored in "counter[i]"; thus
	// the bag { ('a', 6), ('d', 2), ('t', 7)} is represented by:
	//
	// value = { 'a', 'd', 't' }
	// count = { 6 , 2 , 7 }
	// nextFree = 3
	//
	// The remaining positions in arrays "value" and "count" are
	// undefined (most likely null and zero, respectively).

	private T[] value; // keep this array sorted
	private int[] count; // keep only positive counters
	private int nextFree;

	@SuppressWarnings("unchecked")
	public SortedArrayBag() {
		value = (T[]) new Object[INITIAL_CAPACITY];
		count = new int[INITIAL_CAPACITY];
		nextFree = 0;
	}

	private void ensureCapacity() {
		if (nextFree==value.length) {
			value=Arrays.copyOf(value, value.length*2);
			count=Arrays.copyOf(count, count.length*2);
		}
	}

	@Override
	public boolean isEmpty() {
		if (nextFree==0) {
			return true;
		} else {
			return false;
		}
	}

	// search loop:
	// if "item" is stored in the array "value", returns its index
	// otherwise returns the index where "item" would be inserted

	private int locate(T item) {
		int lower = 0;
		int upper = nextFree - 1;
		int mid = 0;
		boolean found = false;

		// binary search
		while (lower <= upper && !found) {
			mid = lower + ((upper - lower) / 2); // == (lower + upper) / 2;
			found = value[mid].equals(item);
			if (!found) {
				if (value[mid].compareTo(item) > 0) {
					upper = mid - 1;
				} else {
					lower = mid + 1;
				}
			}
		}

		if (found)
			return mid; // the index where "item" is stored
		else
			return lower; // the index where "item" would be inserted
	}

	@Override
	public void insert(T item) {
		ensureCapacity();
		int index=locate(item);
		if (item.equals(value[index])) {
			count[index]++;
		} else {
			for (int i=nextFree-1; i>=index; i--) {
					value[i+1]=value[i];
					count[i+1]=count[i];
			}
			value[index]=item;
			count[index]=1;
			nextFree++;
		}
	}

	@Override
	public int occurrences(T item) {
		int i = 0;
		boolean encontrado = false;
		while (i < nextFree && !encontrado) {
			if (value[i].equals(item)) {
				encontrado=true;
			} else {
				i++;
			}
		}
		if (encontrado) {
			return count[i];
		} else {
			return 0;
		}
	}

	@Override
	public void delete(T item) {
		int i =0;
		boolean encontrado = false;
		while (i < nextFree && !encontrado) {
			if (value[i].equals(item)) {
				encontrado=true;
			} else {
				i++;
			}
		}
		if (encontrado) {
			if (count[i]>1) {
				count[i]--;
			} else {
				for (int c=i; c<nextFree; c++) {
					value[i]=value[i+1];
					count[i]=count[i+1];
				}
				nextFree--;
			}
		} else {
			throw new RuntimeException("DELETE ERROR: el elemento a borrar no esta en la bolsa");
		}
	}

	@Override
	public void copyOf(Bag<T> source) {
		// TODO Auto-generated method stub
		// the implementation of 'copyOf' cannot use the iterator
		
	}

	@Override
	public String toString() {
		StringBuffer text = new StringBuffer("ArrayBag( ");
		for (int i = 0; i < nextFree; i++) {
			text.append(String.format("(%s, %s) ", value[i], count[i]));
		}
		return text.append(" )").toString();
	}

	@Override
	public Iterator<T> iterator() {
		// TODO Auto-generated method stub
		return null;
	}
}
