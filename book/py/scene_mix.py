#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
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


class SceneMix(SceneMixin, \
               SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin, \
               SceneNineteenMixin, \
               SceneHemerasDawnMixin, \
               SceneTamoanchanRevisitedMixin, \
               SceneConquestOfTlalocanMixin, \
               SceneDiscoveryMixin, \
               SceneOneMixin):

    # overrides
    def _get_recent_scene_method_names(self):
        return  [
                    # 'scn_mv_43_static_move_is_illegal_init', \
                    # 'scn_mv_44_static_move_is_illegal_end', \
                    # 'scn_mv_45_static_piece_is_legal_init', \
                    # 'scn_mv_46_static_piece_is_legal_end', \
                    # 'scn_mv_47_delayed_promotion_is_legal_init', \
                    # 'scn_mv_48_delayed_promotion_is_legal_end', \
                    # 'scn_mv_56_own_wave_is_divergent_init', \
                    # 'scn_mv_57_own_wave_is_divergent_1', \
                    # 'scn_mv_58_diverging_pawn', \
                    # 'scn_mv_59_diverging_rushing_pawn', \
                    # 'scn_mv_60_diverging_activated_piece_init', \
                    # 'scn_mv_61_diverging_activated_piece_end', \
                    'scn_mv_62_wave_divergence_init', \
                    'scn_mv_63_wave_divergence_1', \
                ]
