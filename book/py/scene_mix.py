#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2020 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE.txt for details.


# from scene import Scene
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin


class SceneMix(SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin, \
               SceneNineteenMixin, \
               SceneHemerasDawnMixin, \
               SceneTamoanchanRevisitedMixin, \
               SceneConquestOfTlalocanMixin):

    def _get_recent_scene_method_names(self):
        return  [
                    'scn_cot_01_shaman_movement', \
                    'scn_cot_02_light_shaman_step_ply', \
                    'scn_cot_03_light_shaman_capture_ply', \
                    'scn_cot_04_dark_shaman_step_ply', \
                    'scn_cot_05_dark_shaman_capture_ply', \

                    'scn_cot_06_wave_activated', \
                    'scn_cot_07_teleport_shaman_all', \
                    'scn_cot_08_teleport_pawn_init', \
                    'scn_cot_09_trance_journey_init', \
                    'scn_cot_10_knight_directions', \

                    'scn_cot_11_stop_sign_pattern', \
                    'scn_cot_12_stop_sign_pattern_unwind', \
                    'scn_cot_13_stop_sign_pattern_full', \
                    'scn_cot_14_light_shaman_trance_journey', \
                    'scn_cot_15_light_shaman_trance_journey_offset', \

                    'scn_cot_16_dark_shaman_trance_journey', \
                    'scn_cot_17_displacement_fields', \
                    'scn_cot_18_light_light_shaman_interaction_start', \
                    'scn_cot_19_light_light_shaman_interaction_end', \
                    'scn_cot_20_dark_light_shaman_interaction_start', \

                    'scn_cot_21_dark_light_shaman_interaction_end', \
                    'scn_cot_22_dark_dark_shaman_interaction_start', \
                    'scn_cot_23_dark_dark_shaman_interaction_end', \
                    'scn_cot_24_dark_dark_shaman_double_interaction_start', \
                    'scn_cot_25_dark_dark_shaman_double_interaction_end', \

                    'scn_cot_26_light_dark_shaman_interaction_start', \
                    'scn_cot_27_light_dark_shaman_interaction_end', \
                    'scn_cot_28_backward_displacement_start', \
                    'scn_cot_29_backward_displacement_end', \
                    'scn_cot_30_forward_displacement_start', \

                    'scn_cot_31_forward_displacement_step_2', \
                    'scn_cot_32_forward_displacement_end', \
                    'scn_cot_33_push_pull_entrancement_start', \
                    'scn_cot_34_push_pull_entrancement_2', \
                    'scn_cot_35_push_pull_entrancement_end', \

                    # 'test_cot_09_stop_sign_pattern_full', \
                ]

    def _get_all_scene_method_names(self):
        return [ n for n in dir(self) if n.startswith('scn_') ] # or n.startswith('move_')

    def _get_attributes(self, names):
        return [ getattr(self, a) for a in names ]

    def get_recent_scene_methods(self):
        return self._get_attributes( self._get_recent_scene_method_names() )

    def get_all_scene_methods(self):
        return self._get_attributes( self._get_all_scene_method_names() )
