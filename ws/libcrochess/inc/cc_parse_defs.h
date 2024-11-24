// Copyright (c) 2024 Mario Mlačak, mmlacak@gmail.com
// Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.

#ifndef __CC_PARSE_DEFS_H__
#define __CC_PARSE_DEFS_H__


// TODO :: DOCS


//
// ply link enum

typedef enum CcPlyLinkEnum {
    CC_PLE_None,
    CC_PLE_StartingPly,
    CC_PLE_CascadingPly,
    CC_PLE_Teleportation,
    CC_PLE_TeleportationReemergence,
    CC_PLE_TeleportationOblation,
    CC_PLE_TranceJourney,
    CC_PLE_DualTranceJourney,
    CC_PLE_FailedTranceJourney,
    CC_PLE_PawnSacrifice,
    CC_PLE_SenseJourney,
    CC_PLE_FailedSenseJourney,
} CcPlyLinkEnum;

#define CC_PLY_LINK_ENUM_IS_ENUMERATOR(ple) ( ( CC_PLE_None <= (ple) ) && ( (ple) <= CC_PLE_FailedSenseJourney ) )

#define CC_PLY_LINK_ENUM_IS_VALID(ple) ( ( CC_PLE_None < (ple) ) && ( (ple) <= CC_PLE_FailedSenseJourney ) )


#define CC_PLY_LINK_ENUM_IS_ANY_TELEPORTATION(ple) ( ( (ple) == CC_PLE_Teleportation )               \
                                                  || ( (ple) == CC_PLE_TeleportationReemergence )    \
                                                  || ( (ple) == CC_PLE_TeleportationOblation )

#define CC_PLY_LINK_ENUM_IS_ANY_TRANCE_JOURNEY(ple) ( ( (ple) == CC_PLE_TranceJourney )          \
                                                   || ( (ple) == CC_PLE_DualTranceJourney )      \
                                                   || ( (ple) == CC_PLE_FailedTranceJourney ) )

#define CC_PLY_LINK_ENUM_IS_ANY_SENSE_JOURNEY(ple) ( ( (ple) == CC_PLE_SenseJourney )        \
                                             || ( (ple) == CC_PLE_FailedSenseJourney )

#define CC_MAX_LEN_PLY_LINK_SYMBOL (3)

char const * cc_ply_link_symbol( CcPlyLinkEnum ple );

//
// step link enum

typedef enum CcStepLinkEnum {
    CC_SLE_None, /* Step link not found, uninitialized, not parsed yet, or error happened. */
    CC_SLE_Start, /* Position from which a piece started moving. */
    CC_SLE_Reposition, /* In trance-journey, dark Shaman's distant starting field; separated by , (comma). */
    CC_SLE_Next, /* Step immediately following previous, separated by . (dot). */
    CC_SLE_Distant, /* Step not immediately following previous, separated by .. (double-dot). */
    CC_SLE_Destination, /* Step to destination field, separated by - (hyphen). */
    CC_SLE_JustDestination, /* Just destination field, no separators, no other steps. */
} CcStepLinkEnum;

#define CC_STEP_LINK_ENUM_IS_ENUMERATOR(sle) ( ( CC_SLE_None <= (sle) ) && ( (sle) <= CC_SLE_JustDestination ) )

#define CC_STEP_LINK_ENUM_IS_VALID(sle) ( ( CC_SLE_None < (sle) ) && ( (sle) <= CC_SLE_JustDestination ) )

#define CC_STEP_LINK_ENUM_IS_DESTINATION(sle) ( ( (sle) == CC_SLE_Destination ) || ( (sle) == CC_SLE_JustDestination ) )

#define CC_MAX_LEN_STEP_LINK_SYMBOL (2)

char const * cc_step_link_symbol( CcStepLinkEnum sle );

//
// side-effect enum

typedef enum CcSideEffectEnum {
    CC_SEE_None, /* No side effects. */
    CC_SEE_Capture, /* Corresponds to `*`. */
    CC_SEE_Displacement, /* Corresponds to `<`. */
    CC_SEE_EnPassant, /* Corresponds to `:`. */
    CC_SEE_Castle, /* Corresponds to `&`. */
    CC_SEE_Promotion, /* Corresponds to `=`, ``. */
    CC_SEE_TagForPromotion, /* Corresponds to `=`. */
    CC_SEE_Conversion, /* Corresponds to `%`. */
    CC_SEE_FailedConversion, /* Corresponds to `%%`. */
    CC_SEE_Transparency, /* Corresponds to `^`. */
    CC_SEE_Divergence, /* Corresponds to `/`. */
    CC_SEE_DemoteToPawn, /* Corresponds to `>`. */
    CC_SEE_Resurrection, /* Corresponds to `$`. */
    CC_SEE_ResurrectingOpponent, /* Corresponds to `$$`. */
    CC_SEE_FailedResurrection, /* Corresponds to `$$$`. */
} CcSideEffectEnum;

// #define CC_PARSED_SIDE_EFFECT_ENUM_IS_CASTLING(see) ( (see) == CC_SEE_Castle )

#define CC_SIDE_EFFECT_ENUM_IS_ENUMERATOR(see) ( ( CC_SEE_None <= (see) ) && ( (see) <= CC_SEE_FailedResurrection ) )

#define CC_SIDE_EFFECT_ENUM_IS_VALID(see) ( ( CC_SEE_None < (see) ) && ( (see) <= CC_SEE_FailedResurrection ) )

#define CC_MAX_LEN_SIDE_EFFECT_SYMBOL (3)

char const * cc_side_effect_symbol( CcSideEffectEnum see );


#endif /* __CC_PARSE_DEFS_H__ */
