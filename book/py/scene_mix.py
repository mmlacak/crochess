#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
from scene_classical_chess import SceneClassicalChessMixin
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin
from scene_discovery import SceneDiscoveryMixin
from scene_one import SceneOneMixin


class SceneMix( SceneMixin, \
                SceneClassicalChessMixin, \
                SceneCroatianTiesMixin, \
                SceneMayanAscendancyMixin, \
                SceneAgeOfAquariusMixin, \
                SceneMirandasVeilMixin, \
                SceneNineteenMixin, \
                SceneHemerasDawnMixin, \
                SceneTamoanchanRevisitedMixin, \
                SceneConquestOfTlalocanMixin, \
                SceneDiscoveryMixin, \
                SceneOneMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    # 'scn_mv_087_en_passant_denied_init', \
                    # 'scn_mv_088_en_passant_denied_pawn_activated', \
                    # 'scn_mv_089_en_passant_denied_end', \
                    # 'scn_mv_090_activation_after_en_passant_init', \
                    # 'scn_mv_091_activation_after_en_passant_end', \
                    # 'scn_mv_092_en_passant_not_blocked_init', \
                    # 'scn_mv_093_en_passant_not_blocked_step_2', \
                    # 'scn_mv_094_en_passant_not_blocked_end', \
                    \
                    # 'scn_mv_097_en_passant_illegal_init', \
                    # 'scn_mv_098_en_passant_illegal_pawn_activated', \
                    # 'scn_mv_099_en_passant_illegal_queen_reactivated', \
                    # 'scn_mv_100_en_passant_illegal_pawn_reactivated', \
                    # # 'scn_mv_095_en_passant_legal_init', \
                    # 'scn_mv_096_en_passant_legal_end', \
                    \
                    # 'scn_n_31_cascading_rushes_en_passants_init', \
                    # 'scn_n_32_cascading_rushes_en_passants_end', \
                    # 'scn_hd_59_en_passant_grenadier_in_close_quarters', \
                    'scn_hd_60_en_passant_grenadier_activated_in_close_quarters', \
                    # 'scn_hd_62_multiple_rushes_init', \
                    # 'scn_hd_63_multiple_rushes_end', \
                ]
