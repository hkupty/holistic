/// Indicates that the file is an extended M3U playlist file.
pub const ExtendedM3U = struct {};

/// Indicates the compatibility version of the playlist file.
pub const Version = struct { number: u8 };

/// Specifies the duration of a media segment.
pub const Inf = struct { duration: f32, title: ?[]const u8 };

/// Indicates that a Media Segment is a sub-range of the resource identified by its URI.
pub const ByteRange = struct { length: usize, offset: ?usize };

/// Indicates a discontinuity between the Media Segment that follows and the one that preceeded it.
pub const Discontinuity = struct {};

pub const KeyAttributeMethod = enum {
    NONE,
    AES_128,
    SAMPLE_AES,
};

pub const KeyAttribute = union(enum) {
    Method: KeyAttributeMethod,
    Uri: []const u8,
    IV: u128,
    Keyformat: []const u8,
    KeyformatVersions: []const u8, // Possibly []u16 as in [1, 2, 5] as a parsed version of "1/2/5"
};

pub const EncriptionKey = struct { attributeList: []const KeyAttribute };

pub const HLSField = union(enum) {
    EXTM3U: ExtendedM3U,
    Version: Version,
    Inf: Inf,
    ByteRange: ByteRange,
    Discontinuity: Discontinuity,
    Key: EncriptionKey,

    // Partial data
    KeyAttribute: KeyAttribute,
};
