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
from scene_simple import SceneSimpleMixin


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
                SceneOneMixin, \
                SceneSimpleMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    # 'scn_ma_04_pyramid_activation_by_pawn', \
                    # 'scn_mv_026_wave_activated_by_step_pawn', \
                    # 'scn_mv_028_wave_activated_by_capture_pawn', \
                    # 'scn_n_16_sideways_pawn', \
                    # 'scn_n_17_sideways_pawn_cannot_activate_pyramid', \
                    # 'scn_n_18_sideways_pawn_can_activate_wave', \
                    # 'scn_n_19_sideways_pawn_activated_wave', \
                    # 'scn_n_20_activating_opponents_wave', \
                    # 'scn_n_21_activated_opponents_wave', \
                    # 'scn_n_22_sideways_pawns_and_activating_pyramid', \
                    # 'scn_hd_15_scout_movement', \
                    # 'scn_hd_23_activating_pyramid', \
                    # 'scn_hd_24_activating_scout_init', \
                    # 'scn_hd_25_activating_scout_end', \
                    # 'scn_hd_26_scout_activating_wave_step_fields_init', \
                    # 'scn_hd_27_scout_activating_wave_step_fields_end', \
                    'scn_hd_28_scout_activating_wave_capture_fields_init', \
                    'scn_hd_29_scout_activating_wave_capture_fields_end', \
                ]
