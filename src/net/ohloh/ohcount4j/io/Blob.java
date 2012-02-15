package net.ohloh.ohcount4j.io;

import java.io.IOException;

public interface Blob {
	/**
	 * Returns path associated with this {@link Blob}.
	 * 
	 * @return path including including the name
	 */
	String getPath();

	/**
	 * Returns name part of the path associated with this {@link Blob} including
	 * extension.
	 * 
	 * @return name including extension
	 * 
	 * @see Blob#getPath()
	 */
	String getName();

	/**
	 * Returns extension associated with name part.
	 * 
	 * It assumes period '.' to be the extension separator character.
	 * 
	 * @return extension
	 * 
	 * @see Blob#getName()
	 */
	String getExtension();

	/**
	 * Returns contents of {@Blob} as character array.
	 * 
	 * This method is stateless. Subsequent calls to peek() or contents() always
	 * return data from the beginning.
	 * 
	 * @return contents
	 * @throws IOException
	 *             If an IO error occurred
	 */
	char[] charContents() throws IOException;

	/**
	 * Returns contents of {@Blob} as byte array.
	 * 
	 * This method is stateless. Subsequent calls to peek() or contents() always
	 * return data from the beginning.
	 * 
	 * @return contents
	 * @throws IOException
	 *             If an IO error occurred
	byte[] byteContents() throws IOException;	 */

	/**
	 * Reads and returns {@code length} number of characters.
	 * 
	 * This method is stateless and doesn't change the start-stream marker. So
	 * subsequent calls to peek() or contents() always return data from the
	 * beginning.
	 * 
	 * @param length
	 *            number of characters to read.
	 * @return
	 * @throws IOException
	 */
	char[] peek(int length) throws IOException;
}
