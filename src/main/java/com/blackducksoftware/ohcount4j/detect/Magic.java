/**
 * Copyright 2016 Black Duck Software, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j.detect;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

// Provides a minimal Java wrapper around the C magic library.
//
// To install libmagic:
//
// OS X: `brew install libmagic`
// Ubuntu: `apt-get install libmagic-dev`
//
public class Magic {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private interface MagicLibrary extends Library {
        MagicLibrary INSTANCE =
                (MagicLibrary) Native.loadLibrary("magic", MagicLibrary.class);

        Pointer magic_open(int flags);

        void magic_close(Pointer cookie);

        String magic_error(Pointer cookie);

        String magic_buffer(Pointer cookie, String buffer, int length);

        String magic_file(Pointer cookie, String filename);

        int magic_load(Pointer cookie, String filename);
    }

    protected Pointer cookie;

    public boolean open(int flags) {
        try {
            cookie = MagicLibrary.INSTANCE.magic_open(flags);
            return true;
        } catch (UnsatisfiedLinkError | NoClassDefFoundError e) {
            // libmagic not available
            logger.warn(e.getMessage());
            return false;
        }
    }

    public boolean open() {
        return open(0);
    }

    public void close() {
        MagicLibrary.INSTANCE.magic_close(cookie);
    }

    public String error() {
        return MagicLibrary.INSTANCE.magic_error(cookie);
    }

    public String buffer(String buf) {
        return MagicLibrary.INSTANCE.magic_buffer(cookie, buf, buf.length());
    }

    public String file(String filename) {
        return MagicLibrary.INSTANCE.magic_file(cookie, filename);
    }

    // Load a magic number database from a file
    public int load(String filename) {
        return MagicLibrary.INSTANCE.magic_load(cookie, filename);
    }

    // Load the default definitions database
    public int load() {
        return load(null);
    }

}
