#!/usr/bin/env python3
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
                    # 'scn_mv_53_rushing_cascade', \
                    # 'scn_mv_54_en_passant_turning_capture', \
                    # 'scn_mv_55_en_passant_turned_capture', \
                    # 'scn_mv_56_en_passant_wave_captured', \
                    # 'scn_mv_64_rushing_cascade_opponent', \
                    # 'scn_mv_65_blocking_en_passant', \
                    # 'scn_mv_66_blocked_en_passant', \
                    # 'scn_mv_67_en_passant_blocked_by_wave', \
                    'scn_tr_19_displacement_init', \
                    'scn_tr_20_displacement_step_1', \
                    'scn_tr_21_displacement_step_2', \
                    'scn_tr_22_displacement_end', \
                    'scn_tr_23_displacement_activated', \
                ]
