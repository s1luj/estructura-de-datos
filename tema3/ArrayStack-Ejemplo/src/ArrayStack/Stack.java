package ArrayStack;

public interface Stack<T> {
	void push(T elem);
	T top() throws ArrayStackException;
	void pop();
	boolean isEmpty();
}
